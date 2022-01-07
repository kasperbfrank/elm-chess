module Main exposing (Msg(..), calculatePawnMoves, main, update, view)

import Arithmetic as Math
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)


type Piece
    = Pawn Color Int
    | Rook Color Int
    | Knight Color Int
    | Bishop Color Int
    | Queen Color Int
    | King Color Int


type Color
    = White
    | Black


type MoveCount
    = SingleMove
    | UnlimitedMoves


type alias Model =
    { board : BoardState
    , selection : Maybe Selection
    , possibleMoves : List Position
    , turn : Color
    , victory : Maybe Color
    }


type alias BoardState =
    Dict Position Piece


type Msg
    = ClickedFieldWithPiece Piece Position
    | ClickedFieldWithMove Move
    | ClickedPlayAgainButton


type alias Selection =
    { piece : Piece, position : Position }


type alias Move =
    { piece : Piece, from : Position, to : Position }


type alias Position =
    ( Int, Int )


incrementMoves : Piece -> Piece
incrementMoves piece =
    case piece of
        Pawn color moveCount ->
            Pawn color (moveCount + 1)

        Rook color moveCount ->
            Rook color (moveCount + 1)

        Knight color moveCount ->
            Knight color (moveCount + 1)

        Bishop color moveCount ->
            Bishop color (moveCount + 1)

        Queen color moveCount ->
            Queen color (moveCount + 1)

        King color moveCount ->
            King color (moveCount + 1)


isFirstMove : Piece -> Bool
isFirstMove piece =
    case piece of
        Pawn _ moveCount ->
            moveCount == 0

        Rook _ moveCount ->
            moveCount == 0

        Knight _ moveCount ->
            moveCount == 0

        Bishop _ moveCount ->
            moveCount == 0

        Queen _ moveCount ->
            moveCount == 0

        King _ moveCount ->
            moveCount == 0


createPieceFromInitPosition : Position -> Maybe Piece
createPieceFromInitPosition ( row, col ) =
    case row of
        1 ->
            case col of
                1 ->
                    Just (Rook White 0)

                2 ->
                    Just (Knight White 0)

                3 ->
                    Just (Bishop White 0)

                4 ->
                    Just (Queen White 0)

                5 ->
                    Just (King White 0)

                6 ->
                    Just (Bishop White 0)

                7 ->
                    Just (Knight White 0)

                8 ->
                    Just (Rook White 0)

                _ ->
                    Nothing

        2 ->
            Just (Pawn White 0)

        7 ->
            Just (Pawn Black 0)

        8 ->
            case col of
                1 ->
                    Just (Rook Black 0)

                2 ->
                    Just (Knight Black 0)

                3 ->
                    Just (Bishop Black 0)

                4 ->
                    Just (King Black 0)

                5 ->
                    Just (Queen Black 0)

                6 ->
                    Just (Bishop Black 0)

                7 ->
                    Just (Knight Black 0)

                8 ->
                    Just (Rook Black 0)

                _ ->
                    Nothing

        _ ->
            Nothing


createPieceWithIndexTuple : Position -> Maybe ( Position, Piece )
createPieceWithIndexTuple position =
    let
        maybePiece : Maybe Piece
        maybePiece =
            createPieceFromInitPosition position
    in
    Maybe.map (Tuple.pair position) maybePiece


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
        ClickedFieldWithPiece piece position ->
            let
                ( selectedPiece, possibleMoves ) =
                    if Just piece == Maybe.map .piece model.selection then
                        ( Nothing, [] )

                    else
                        ( Just (Selection piece position)
                        , calculatePossibleMoves model.board True piece position
                        )
            in
            ( { model
                | selection = selectedPiece
                , possibleMoves = possibleMoves
              }
            , Cmd.none
            )

        ClickedFieldWithMove ({ piece, from, to } as move) ->
            let
                newBoardState : Dict Position Piece
                newBoardState =
                    doMove model.board move

                isOtherKing : Piece -> Bool
                isOtherKing piece_ =
                    case piece_ of
                        King color _ ->
                            color == otherColor model.turn

                        _ ->
                            False

                enemyKing : Maybe Piece
                enemyKing =
                    Dict.values newBoardState
                        |> List.filter isOtherKing
                        |> List.head
            in
            ( { model
                | board = newBoardState
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
doMove boardState { piece, from, to } =
    boardState
        |> Dict.remove from
        |> Dict.insert to (incrementMoves piece)


otherColor : Color -> Color
otherColor color =
    case color of
        White ->
            Black

        Black ->
            White


calculatePossibleMoves : BoardState -> Bool -> Piece -> Position -> List Position
calculatePossibleMoves boardState checkKingMoves piece position =
    let
        allMoves : Color -> MoveCount -> List (Position -> Position) -> List Position
        allMoves color moveCount moveFns =
            List.foldl
                (\moveFn acc -> calculateMovesFromPosition boardState position color moveCount moveFn ++ acc)
                []
                moveFns
    in
    case piece of
        Pawn color _ ->
            let
                direction : Int -> Int -> Int
                direction =
                    case color of
                        White ->
                            (+)

                        Black ->
                            (-)
            in
            calculatePawnMoves boardState color direction position piece

        Rook color _ ->
            allMoves
                color
                UnlimitedMoves
                straightMoves

        Knight color _ ->
            allMoves
                color
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

        Bishop color _ ->
            allMoves
                color
                UnlimitedMoves
                diagonalMoves

        Queen color _ ->
            allMoves
                color
                UnlimitedMoves
                (straightMoves ++ diagonalMoves)

        (King color moveCount) as king ->
            let
                moves : List Position
                moves =
                    allMoves
                        color
                        SingleMove
                        (straightMoves ++ diagonalMoves)
            in
            if checkKingMoves then
                moves
                    |> List.map (Move king position)
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


calculateAllMovesForColor : BoardState -> Color -> List Position
calculateAllMovesForColor boardState color =
    let
        maybePair : Position -> Maybe ( Position, Piece )
        maybePair key =
            Dict.get key boardState |> Maybe.map (Tuple.pair key)
    in
    Dict.keys boardState
        |> List.filterMap maybePair
        |> List.filter (Tuple.second >> getColor >> (/=) color)
        |> List.concatMap
            (\tuple ->
                calculatePossibleMoves
                    boardState
                    False
                    (Tuple.second tuple)
                    (Tuple.first tuple)
            )


diagonalMoves : List (Position -> Position)
diagonalMoves =
    [ \( row, col ) -> ( row + 1, col + 1 )
    , \( row, col ) -> ( row + 1, col - 1 )
    , \( row, col ) -> ( row - 1, col - 1 )
    , \( row, col ) -> ( row - 1, col + 1 )
    ]


straightMoves : List (Position -> Position)
straightMoves =
    [ \( row, col ) -> ( row + 1, col )
    , \( row, col ) -> ( row - 1, col )
    , \( row, col ) -> ( row, col + 1 )
    , \( row, col ) -> ( row, col - 1 )
    ]


calculateMovesFromPosition : BoardState -> Position -> Color -> MoveCount -> (Position -> Position) -> List Position
calculateMovesFromPosition boardState startPosition playerColor moveCount tryMove =
    let
        move : List Position -> Position -> List Position
        move acc from =
            let
                to : Position
                to =
                    tryMove from

                inhabitant : Maybe Piece
                inhabitant =
                    Dict.get to boardState
            in
            if isPositionOnBoard to then
                case inhabitant of
                    Just piece ->
                        if getColor piece == playerColor then
                            acc

                        else
                            -- Other player has a unit on this field,
                            -- so it's a valid move, but cannot go further
                            to :: acc

                    Nothing ->
                        if moveCount == SingleMove then
                            to :: acc

                        else
                            -- find next move
                            move (to :: acc) to

            else
                acc
    in
    move [] startPosition


isPositionOnBoard : Position -> Bool
isPositionOnBoard ( row, col ) =
    0 < row && row < 9 && 0 < col && col < 9


calculatePawnMoves : BoardState -> Color -> (Int -> Int -> Int) -> Position -> Piece -> List Position
calculatePawnMoves boardState playerColor direction ( row, col ) piece =
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
            if isFirstMove piece && moveOne /= Nothing then
                [ Just ( direction row 2, col ), moveOne ]

            else
                [ moveOne ]

        destroyMoves : List (Maybe ( Int, Int ))
        destroyMoves =
            -- Pawn can destroy another unit by moving one forward diagonally
            let
                maybeDestroyMove : Position -> Maybe Position
                maybeDestroyMove move =
                    Dict.get move boardState
                        |> maybeWhen (getColor >> (/=) playerColor)
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

        maybePiece : Maybe Piece
        maybePiece =
            Dict.get ( rowIndex, colIndex ) model.board

        isMove : Bool
        isMove =
            List.any ((==) ( rowIndex, colIndex )) model.possibleMoves

        isSelectedField : Bool
        isSelectedField =
            Maybe.map .position model.selection == Just ( rowIndex, colIndex )

        styles : List (Attribute msg)
        styles =
            [ Attr.class "h-20 w-20 flex justify-center items-center text-3xl font-bold relative"
            , Attr.class
                (if isSelectedField then
                    "transition-all border-4 border-cyan-400 text-cyan-400"

                 else
                    case maybePiece of
                        Just piece ->
                            "text-" ++ String.toLower (colorToString (getColor piece))

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
                    Just { piece, position } ->
                        [ onClick (ClickedFieldWithMove (Move piece position ( rowIndex, colIndex ))) ]

                    Nothing ->
                        []

            else
                case maybePiece of
                    Just piece ->
                        if getColor piece == model.turn then
                            [ onClick (ClickedFieldWithPiece piece ( rowIndex, colIndex )) ]

                        else
                            []

                    Nothing ->
                        []
    in
    Html.div
        (styles ++ eventHandlers)
        (viewPieceAndMove maybePiece isMove)


getColor : Piece -> Color
getColor piece =
    case piece of
        Pawn color _ ->
            color

        Rook color _ ->
            color

        Knight color _ ->
            color

        Bishop color _ ->
            color

        Queen color _ ->
            color

        King color _ ->
            color


viewPieceAndMove : Maybe Piece -> Bool -> List (Html msg)
viewPieceAndMove maybePiece isMove =
    case ( maybePiece, isMove ) of
        ( Just piece, True ) ->
            [ Html.text (pieceIcon piece)
            , Html.div [ Attr.class "scale-150 absolute w-full h-full text-red-500 flex justify-center items-center" ] [ Html.text "X" ]
            ]

        ( Just piece, False ) ->
            [ Html.text (pieceIcon piece) ]

        ( Nothing, True ) ->
            [ Html.text "o" ]

        ( Nothing, False ) ->
            [ Html.text "" ]


pieceIcon : Piece -> String
pieceIcon piece =
    case piece of
        Pawn _ _ ->
            "P"

        Rook _ _ ->
            "R"

        Knight _ _ ->
            "H"

        Bishop _ _ ->
            "B"

        Queen _ _ ->
            "Q"

        King _ _ ->
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
