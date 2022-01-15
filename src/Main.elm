module Main exposing (Msg(..), calculatePawnMoves, main, update, view)

import Arithmetic as Math
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)


type Piece
    = Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King


type Color
    = White
    | Black


type MoveRestriction
    = SingleMove
    | UnlimitedMoves


type alias PieceDetails =
    { piece : Piece
    , color : Color
    , moveCount : Int
    }


type alias Model =
    { board : BoardState
    , history : List Move
    , selection : Maybe Selection
    , possibleMoves : List Square
    , turn : Color
    , victory : Maybe Color
    }


type alias BoardState =
    Dict Square PieceDetails


type Msg
    = ClickedFieldWithPiece PieceDetails Square
    | ClickedFieldWithMove Move
    | ClickedPlayAgainButton


type alias Selection =
    { piece : PieceDetails, square : Square }


type alias Move =
    { pieceDetails : PieceDetails, from : Square, to : Square }


type alias Square =
    ( Int, Int )


createPieceFromInitSquare : Square -> Maybe PieceDetails
createPieceFromInitSquare ( row, col ) =
    case row of
        1 ->
            case col of
                1 ->
                    Just (PieceDetails Rook White 0)

                2 ->
                    Just (PieceDetails Knight White 0)

                3 ->
                    Just (PieceDetails Bishop White 0)

                4 ->
                    Just (PieceDetails Queen White 0)

                5 ->
                    Just (PieceDetails King White 0)

                6 ->
                    Just (PieceDetails Bishop White 0)

                7 ->
                    Just (PieceDetails Knight White 0)

                8 ->
                    Just (PieceDetails Rook White 0)

                _ ->
                    Nothing

        2 ->
            Just (PieceDetails Pawn White 0)

        7 ->
            Just (PieceDetails Pawn Black 0)

        8 ->
            case col of
                1 ->
                    Just (PieceDetails Rook Black 0)

                2 ->
                    Just (PieceDetails Knight Black 0)

                3 ->
                    Just (PieceDetails Bishop Black 0)

                4 ->
                    Just (PieceDetails King Black 0)

                5 ->
                    Just (PieceDetails Queen Black 0)

                6 ->
                    Just (PieceDetails Bishop Black 0)

                7 ->
                    Just (PieceDetails Knight Black 0)

                8 ->
                    Just (PieceDetails Rook Black 0)

                _ ->
                    Nothing

        _ ->
            Nothing


createPieceWithIndexTuple : Square -> Maybe ( Square, PieceDetails )
createPieceWithIndexTuple square =
    let
        maybePiece : Maybe PieceDetails
        maybePiece =
            createPieceFromInitSquare square
    in
    Maybe.map (Tuple.pair square) maybePiece


initBoardState : BoardState
initBoardState =
    List.range 1 8
        |> List.map (\n -> ( n, List.range 1 8 ))
        |> List.concatMap
            (\( rowIndex, colIndexes ) -> List.map (Tuple.pair rowIndex) colIndexes)
        |> List.map createPieceWithIndexTuple
        |> List.filterMap identity
        |> Dict.fromList


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = initBoardState
      , history = []
      , selection = Nothing
      , possibleMoves = []
      , turn = White
      , victory = Nothing
      }
    , Cmd.none
    )



------------------------------------------------------------------------------------------------------------------------
-- UPDATE
------------------------------------------------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFieldWithPiece piece square ->
            let
                ( selectedPiece, possibleMoves ) =
                    if Just piece == Maybe.map .piece model.selection then
                        ( Nothing, [] )

                    else
                        ( Just (Selection piece square)
                        , calculatePossibleMoves model.board True piece square
                        )
            in
            ( { model
                | selection = selectedPiece
                , possibleMoves = possibleMoves
              }
            , Cmd.none
            )

        ClickedFieldWithMove ({ pieceDetails, from, to } as move) ->
            let
                newBoardState : BoardState
                newBoardState =
                    doMove model.board move

                isOtherKing : PieceDetails -> Bool
                isOtherKing { piece, color } =
                    case piece of
                        King ->
                            color == otherColor model.turn

                        _ ->
                            False

                enemyKing : Maybe PieceDetails
                enemyKing =
                    Dict.values newBoardState
                        |> List.filter isOtherKing
                        |> List.head
            in
            ( { model
                | board = newBoardState
                , history = move :: model.history
                , possibleMoves = []
                , selection = Nothing
                , turn = otherColor model.turn
                , victory = invertMap model.turn enemyKing
              }
            , Cmd.none
            )

        ClickedPlayAgainButton ->
            init ()


doMove : BoardState -> Move -> BoardState
doMove boardState { pieceDetails, from, to } =
    boardState
        |> Dict.remove from
        |> Dict.insert to { pieceDetails | moveCount = pieceDetails.moveCount + 1 }


otherColor : Color -> Color
otherColor color =
    case color of
        White ->
            Black

        Black ->
            White


calculatePossibleMoves : BoardState -> Bool -> PieceDetails -> Square -> List Square
calculatePossibleMoves boardState checkKingMoves ({ piece, color, moveCount } as pieceDetails) square =
    let
        allMoves : MoveRestriction -> List (Square -> Square) -> List Square
        allMoves moveRestriction moveFns =
            List.foldl
                (\moveFn acc -> calculateMovesFromSquare boardState square color moveRestriction moveFn ++ acc)
                []
                moveFns
    in
    case piece of
        Pawn ->
            let
                direction : Int -> Int -> Int
                direction =
                    case color of
                        White ->
                            (+)

                        Black ->
                            (-)
            in
            calculatePawnMoves boardState color direction square pieceDetails

        Rook ->
            allMoves
                UnlimitedMoves
                straightMoves

        Knight ->
            allMoves
                SingleMove
                [ \( row, col ) -> ( row + 2, col + 1 )
                , \( row, col ) -> ( row + 1, col + 2 )
                , \( row, col ) -> ( row - 1, col + 2 )
                , \( row, col ) -> ( row - 2, col + 1 )
                , \( row, col ) -> ( row - 2, col - 1 )
                , \( row, col ) -> ( row - 1, col - 2 )
                , \( row, col ) -> ( row + 1, col - 2 )
                , \( row, col ) -> ( row + 2, col - 1 )
                ]

        Bishop ->
            allMoves
                UnlimitedMoves
                diagonalMoves

        Queen ->
            allMoves
                UnlimitedMoves
                (straightMoves ++ diagonalMoves)

        King ->
            let
                moves : List Square
                moves =
                    allMoves
                        SingleMove
                        (straightMoves ++ diagonalMoves)
            in
            if checkKingMoves then
                moves
                    |> List.map (Move pieceDetails square)
                    |> List.filter (isMoveSafe boardState color)
                    |> List.map .to

            else
                moves


isMoveSafe : BoardState -> Color -> Move -> Bool
isMoveSafe boardState color move =
    not <|
        List.member
            move.to
            (calculateAllMovesForColor (doMove boardState move) (otherColor color))


calculateAllMovesForColor : BoardState -> Color -> List Square
calculateAllMovesForColor boardState color =
    let
        maybePair : Square -> Maybe ( Square, PieceDetails )
        maybePair key =
            Dict.get key boardState |> Maybe.map (Tuple.pair key)
    in
    Dict.keys boardState
        |> List.filterMap maybePair
        |> List.filter (Tuple.second >> .color >> (/=) color)
        |> List.concatMap
            (\tuple ->
                calculatePossibleMoves
                    boardState
                    False
                    (Tuple.second tuple)
                    (Tuple.first tuple)
            )


diagonalMoves : List (Square -> Square)
diagonalMoves =
    [ \( row, col ) -> ( row + 1, col + 1 )
    , \( row, col ) -> ( row + 1, col - 1 )
    , \( row, col ) -> ( row - 1, col - 1 )
    , \( row, col ) -> ( row - 1, col + 1 )
    ]


straightMoves : List (Square -> Square)
straightMoves =
    [ \( row, col ) -> ( row + 1, col )
    , \( row, col ) -> ( row - 1, col )
    , \( row, col ) -> ( row, col + 1 )
    , \( row, col ) -> ( row, col - 1 )
    ]


calculateMovesFromSquare : BoardState -> Square -> Color -> MoveRestriction -> (Square -> Square) -> List Square
calculateMovesFromSquare boardState fromSquare playerColor moveRestriction tryMove =
    let
        move : List Square -> Square -> List Square
        move acc from =
            let
                to : Square
                to =
                    tryMove from

                inhabitant : Maybe PieceDetails
                inhabitant =
                    Dict.get to boardState
            in
            if isSquareOnBoard to then
                case inhabitant of
                    Just { color } ->
                        if color == playerColor then
                            acc

                        else
                            -- Other player has a unit on this field,
                            -- so it's a valid move, but cannot go further
                            to :: acc

                    Nothing ->
                        if moveRestriction == SingleMove then
                            to :: acc

                        else
                            -- find next move
                            move (to :: acc) to

            else
                acc
    in
    move [] fromSquare


isSquareOnBoard : Square -> Bool
isSquareOnBoard ( row, col ) =
    0 < row && row < 9 && 0 < col && col < 9


calculatePawnMoves : BoardState -> Color -> (Int -> Int -> Int) -> Square -> PieceDetails -> List Square
calculatePawnMoves boardState playerColor direction ( row, col ) { moveCount } =
    let
        baseMoves : List (Maybe ( Int, Int ))
        baseMoves =
            let
                oneForward : ( Int, Int )
                oneForward =
                    ( direction row 1, col )

                moveOne : Maybe ( Int, Int )
                moveOne =
                    -- Pawns cannot destroy another unit on a regular move straight forward
                    if Dict.get oneForward boardState == Nothing then
                        Just oneForward

                    else
                        Nothing
            in
            if moveCount == 0 && moveOne /= Nothing then
                [ Just ( direction row 2, col ), moveOne ]

            else
                [ moveOne ]

        destroyMoves : List (Maybe ( Int, Int ))
        destroyMoves =
            -- Pawn can destroy another unit by moving one forward diagonally
            let
                maybeDestroyMove : Square -> Maybe Square
                maybeDestroyMove move =
                    Dict.get move boardState
                        |> maybeWhen (.color >> (/=) playerColor)
                        |> Maybe.map (\_ -> move)
            in
            [ maybeDestroyMove ( direction row 1, col - 1 )
            , maybeDestroyMove ( direction row 1, col + 1 )
            ]
    in
    List.filterMap identity (baseMoves ++ destroyMoves)



------------------------------------------------------------------------------------------------------------------------
-- VIEW
------------------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.main_ [ Attr.class "w-full h-full flex justify-center items-center select-none" ]
        [ case model.victory of
            Just color ->
                viewVictory color

            Nothing ->
                viewBoard model
        ]


viewVictory : Color -> Html Msg
viewVictory color =
    Html.div
        [ Attr.class "flex flex-col items-center" ]
        [ Html.h1
            [ Attr.class "text-3xl mb-4" ]
            [ Html.text ("Player " ++ colorToString color ++ " is victorious! 🤴🏼") ]
        , Html.button
            [ Events.onClick ClickedPlayAgainButton
            , Attr.class "text-xl bg-emerald-500 rounded text-white p-4 hover:bg-emerald-600 transition-all hover:scale-110"
            ]
            [ Html.text "Play again!" ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    Html.section
        [ Attr.class "border-4 border-slate-800" ]
        (List.reverse (List.range 1 8) |> List.map (viewRow model))


viewRow : Model -> Int -> Html Msg
viewRow model rowIndex =
    Html.div [ Attr.class "flex" ]
        (List.range 1 8 |> List.map (viewCell model rowIndex))


viewCell : Model -> Int -> Int -> Html Msg
viewCell model rowIndex colIndex =
    let
        cellClass : Attribute msg
        cellClass =
            Attr.class <|
                if Math.isEven rowIndex == Math.isEven colIndex then
                    "bg-slate-700"

                else
                    "bg-slate-200"

        maybePiece : Maybe PieceDetails
        maybePiece =
            Dict.get ( rowIndex, colIndex ) model.board

        isMove : Bool
        isMove =
            List.any ((==) ( rowIndex, colIndex )) model.possibleMoves

        isSelectedField : Bool
        isSelectedField =
            Maybe.map .square model.selection == Just ( rowIndex, colIndex )

        styles : List (Attribute msg)
        styles =
            [ Attr.class "h-20 w-20 flex justify-center items-center text-3xl font-bold relative"
            , Attr.class
                (if isSelectedField then
                    "transition-all border-4 border-cyan-400 text-cyan-400"

                 else
                    case maybePiece of
                        Just { color } ->
                            "text-" ++ String.toLower (colorToString color)

                        Nothing ->
                            if isMove then
                                "text-emerald-400"

                            else
                                ""
                )
            , cellClass
            ]

        eventHandlers : List (Attribute Msg)
        eventHandlers =
            if isMove then
                case model.selection of
                    Just { piece, square } ->
                        [ onClick (ClickedFieldWithMove (Move piece square ( rowIndex, colIndex ))) ]

                    Nothing ->
                        []

            else
                case maybePiece of
                    Just ({ piece, color } as pieceDetails) ->
                        if color == model.turn then
                            [ onClick (ClickedFieldWithPiece pieceDetails ( rowIndex, colIndex )) ]

                        else
                            []

                    Nothing ->
                        []
    in
    Html.div
        (styles ++ eventHandlers)
        (viewPieceAndMove maybePiece isMove)


viewPieceAndMove : Maybe PieceDetails -> Bool -> List (Html msg)
viewPieceAndMove maybePieceDetails isMove =
    case ( maybePieceDetails, isMove ) of
        ( Just { piece }, True ) ->
            [ Html.text (pieceIcon piece)
            , Html.div [ Attr.class "scale-150 absolute w-full h-full text-red-500 flex justify-center items-center" ] [ Html.text "X" ]
            ]

        ( Just { piece }, False ) ->
            [ Html.text (pieceIcon piece) ]

        ( Nothing, True ) ->
            [ Html.text "o" ]

        ( Nothing, False ) ->
            [ Html.text "" ]


pieceIcon : Piece -> String
pieceIcon piece =
    case piece of
        Pawn ->
            "P"

        Rook ->
            "R"

        Knight ->
            "H"

        Bishop ->
            "B"

        Queen ->
            "Q"

        King ->
            "K"


colorToString : Color -> String
colorToString color =
    case color of
        White ->
            "White"

        Black ->
            "Black"


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



------------------------------------------------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------------------------------------------------


maybeWhen : (a -> Bool) -> Maybe a -> Maybe a
maybeWhen predicate maybe =
    case maybe of
        Just arg ->
            if predicate arg then
                maybe

            else
                Nothing

        Nothing ->
            Nothing


invertMap : a -> Maybe b -> Maybe a
invertMap ifNothing maybe =
    case maybe of
        Just _ ->
            Nothing

        Nothing ->
            Just ifNothing
