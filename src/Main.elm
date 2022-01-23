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
    , startSquare : Square
    }


type alias Model =
    { board : BoardState
    , moveHistory : List Move
    , selection : Maybe Selection
    , possibleMoves : List Move
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
    { type_ : MoveType, pieceDetails : PieceDetails, from : Square, to : Square }


type MoveType
    = RegularMove
    | EnPassantMove


type alias Square =
    ( Int, Int )


createPieceFromInitSquare : Square -> Maybe PieceDetails
createPieceFromInitSquare ( row, col ) =
    case row of
        1 ->
            case col of
                1 ->
                    Just (PieceDetails Rook White ( row, col ))

                2 ->
                    Just (PieceDetails Knight White ( row, col ))

                3 ->
                    Just (PieceDetails Bishop White ( row, col ))

                4 ->
                    Just (PieceDetails Queen White ( row, col ))

                5 ->
                    Just (PieceDetails King White ( row, col ))

                6 ->
                    Just (PieceDetails Bishop White ( row, col ))

                7 ->
                    Just (PieceDetails Knight White ( row, col ))

                8 ->
                    Just (PieceDetails Rook White ( row, col ))

                _ ->
                    Nothing

        2 ->
            Just (PieceDetails Pawn White ( row, col ))

        7 ->
            Just (PieceDetails Pawn Black ( row, col ))

        8 ->
            case col of
                1 ->
                    Just (PieceDetails Rook Black ( row, col ))

                2 ->
                    Just (PieceDetails Knight Black ( row, col ))

                3 ->
                    Just (PieceDetails Bishop Black ( row, col ))

                4 ->
                    Just (PieceDetails King Black ( row, col ))

                5 ->
                    Just (PieceDetails Queen Black ( row, col ))

                6 ->
                    Just (PieceDetails Bishop Black ( row, col ))

                7 ->
                    Just (PieceDetails Knight Black ( row, col ))

                8 ->
                    Just (PieceDetails Rook Black ( row, col ))

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
      , moveHistory = []
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
                        , calculatePossibleMoves model.board model.moveHistory True piece square
                        )
            in
            ( { model
                | selection = selectedPiece
                , possibleMoves = possibleMoves
              }
            , Cmd.none
            )

        ClickedFieldWithMove move ->
            let
                boardStateAfterMove : BoardState
                boardStateAfterMove =
                    doMove model.board move

                newBoardState : BoardState
                newBoardState =
                    case move.type_ of
                        RegularMove ->
                            boardStateAfterMove

                        EnPassantMove ->
                            case List.head model.moveHistory of
                                Just { to } ->
                                    Dict.remove to boardStateAfterMove

                                Nothing ->
                                    boardStateAfterMove

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
                , moveHistory = move :: model.moveHistory
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
        |> Dict.insert to pieceDetails


otherColor : Color -> Color
otherColor color =
    case color of
        White ->
            Black

        Black ->
            White


calculatePossibleMoves : BoardState -> List Move -> Bool -> PieceDetails -> Square -> List Move
calculatePossibleMoves boardState moveHistory checkKingMoves ({ piece, color } as pieceDetails) square =
    let
        regularMoves : MoveRestriction -> List (Square -> Square) -> List Move
        regularMoves moveRestriction moveFns =
            List.foldl
                (\moveFn acc -> calculateMoveDestinationsFromSquare boardState square color moveRestriction moveFn ++ acc)
                []
                moveFns
                |> List.map (Move RegularMove pieceDetails square)
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
            calculatePawnMoves boardState moveHistory color direction square pieceDetails

        Rook ->
            regularMoves
                UnlimitedMoves
                straightMoves

        Knight ->
            regularMoves
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
            regularMoves
                UnlimitedMoves
                diagonalMoves

        Queen ->
            regularMoves
                UnlimitedMoves
                (straightMoves ++ diagonalMoves)

        King ->
            let
                moves : List Move
                moves =
                    regularMoves
                        SingleMove
                        (straightMoves ++ diagonalMoves)
            in
            if checkKingMoves then
                List.filter (isMoveSafe boardState moveHistory color) moves

            else
                moves


isMoveSafe : BoardState -> List Move -> Color -> Move -> Bool
isMoveSafe boardState moveHistory color move =
    not <|
        List.member
            move
            (calculateAllMovesForColor (doMove boardState move) moveHistory (otherColor color))


calculateAllMovesForColor : BoardState -> List Move -> Color -> List Move
calculateAllMovesForColor boardState moveHistory color =
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
                    moveHistory
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


calculateMoveDestinationsFromSquare : BoardState -> Square -> Color -> MoveRestriction -> (Square -> Square) -> List Square
calculateMoveDestinationsFromSquare boardState fromSquare playerColor moveRestriction tryMove =
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


hasMovedTimes : Int -> List Move -> PieceDetails -> Bool
hasMovedTimes times moveHistory piece =
    List.length (List.filter (.pieceDetails >> (==) piece) moveHistory) == times


calculatePawnMoves : BoardState -> List Move -> Color -> (Int -> Int -> Int) -> Square -> PieceDetails -> List Move
calculatePawnMoves boardState moveHistory playerColor advanceDirection (( row, col ) as currentSquare) pieceDetails =
    let
        advanceOne : Square -> Square
        advanceOne ( row_, col_ ) =
            ( advanceDirection row_ 1, col_ )

        move : MoveType -> Square -> Maybe Move
        move moveType destination =
            Just
                { type_ = moveType
                , pieceDetails = pieceDetails
                , from = currentSquare
                , to = destination
                }

        baseMoves : List (Maybe Move)
        baseMoves =
            let
                moveOne : Maybe Move
                moveOne =
                    -- Pawns cannot destroy another unit on a regular move straight forward
                    if Dict.get (advanceOne currentSquare) boardState == Nothing then
                        move RegularMove (advanceOne currentSquare)

                    else
                        Nothing
            in
            if hasMovedTimes 0 moveHistory pieceDetails && moveOne /= Nothing then
                [ move RegularMove ( advanceDirection row 2, col ), moveOne ]

            else
                [ moveOne ]

        destroyMoves : List (Maybe Move)
        destroyMoves =
            -- Pawn can destroy another unit by moving one forward diagonally
            let
                maybeDestroyMove : (Int -> Int -> Int) -> Maybe Move
                maybeDestroyMove horizontalDirection =
                    let
                        destination =
                            ( advanceDirection row 1, horizontalDirection col 1 )
                    in
                    Dict.get destination boardState
                        |> maybeWhen (.color >> (/=) playerColor)
                        |> Maybe.map
                            (always
                                { type_ = RegularMove
                                , pieceDetails = pieceDetails
                                , from = currentSquare
                                , to = destination
                                }
                            )

                -- En passant requirements
                -- 1. The capturing pawn must have advanced exactly three ranks to perform this move.
                -- 2. The captured pawn must have moved two squares in one move, landing right next to the capturing pawn.
                -- 3. The en passant capture must be performed on the turn immediately after the pawn being captured moves. If the player does not capture en passant on that turn, they no longer can do it later.
                maybeEnPassantMove : (Int -> Int -> Int) -> Maybe Move
                maybeEnPassantMove horizontalDirection =
                    let
                        previousMove : Maybe Move
                        previousMove =
                            List.head moveHistory
                    in
                    case previousMove of
                        Just move_ ->
                            if
                                (pieceDetails.startSquare |> advanceOne |> advanceOne |> advanceOne)
                                    == currentSquare
                                    && move_.pieceDetails.piece
                                    == Pawn
                                    && hasMovedTimes 1 moveHistory move_.pieceDetails
                                    && move_.to
                                    == ( row, horizontalDirection col 1 )
                            then
                                Just
                                    { type_ = EnPassantMove
                                    , pieceDetails = pieceDetails
                                    , from = currentSquare
                                    , to = ( advanceDirection row 1, horizontalDirection col 1 )
                                    }

                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
            [ maybeDestroyMove (-)
            , maybeDestroyMove (+)
            , maybeEnPassantMove (-)
            , maybeEnPassantMove (+)
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

        maybeMove : Maybe Move
        maybeMove =
            model.possibleMoves
                |> List.filter (.to >> (==) ( rowIndex, colIndex ))
                |> List.head

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
                            case maybeMove of
                                Just _ ->
                                    "text-emerald-400"

                                Nothing ->
                                    ""
                )
            , cellClass
            ]

        eventHandlers : List (Attribute Msg)
        eventHandlers =
            case maybeMove of
                Just move ->
                    case model.selection of
                        Just { piece, square } ->
                            [ onClick (ClickedFieldWithMove move) ]

                        Nothing ->
                            []

                Nothing ->
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
        (viewPieceAndMove maybePiece (maybeMove /= Nothing))


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
