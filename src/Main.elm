module Main exposing (Msg(..), calculatePawnMoves, main, update, view)

import Arithmetic as Math
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)


type PieceType
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


type CastlingDirection
    = Kingside
    | Queenside


type alias Piece =
    { type_ : PieceType
    , color : Color
    , startSquare : Square
    }


type alias Model =
    { board : BoardState
    , moveStack : List Move
    , selection : Maybe Selection
    , possibleMoves : List Move
    , turn : Color
    , victory : Maybe Color
    }


type alias BoardState =
    Dict Square Piece


type Msg
    = ClickedFieldWithPiece Piece Square
    | ClickedFieldWithMove Move
    | ClickedPlayAgainButton


type alias Selection =
    { piece : Piece, square : Square }


type Move
    = RegularMove MoveDetails
    | EnPassantMove { moveDetails : MoveDetails, squareToClear : Square }
    | CastlingMove { kingMove : MoveDetails, rookMove : MoveDetails }


type alias MoveDetails =
    { piece : Piece, from : Square, to : Square }


type alias Square =
    ( Int, Int )


createPieceFromInitSquare : Square -> Maybe Piece
createPieceFromInitSquare ( row, col ) =
    case row of
        1 ->
            case col of
                1 ->
                    Just (Piece Rook White ( row, col ))

                2 ->
                    Just (Piece Knight White ( row, col ))

                3 ->
                    Just (Piece Bishop White ( row, col ))

                4 ->
                    Just (Piece Queen White ( row, col ))

                5 ->
                    Just (Piece King White ( row, col ))

                6 ->
                    Just (Piece Bishop White ( row, col ))

                7 ->
                    Just (Piece Knight White ( row, col ))

                8 ->
                    Just (Piece Rook White ( row, col ))

                _ ->
                    Nothing

        2 ->
            Just (Piece Pawn White ( row, col ))

        7 ->
            Just (Piece Pawn Black ( row, col ))

        8 ->
            case col of
                1 ->
                    Just (Piece Rook Black ( row, col ))

                2 ->
                    Just (Piece Knight Black ( row, col ))

                3 ->
                    Just (Piece Bishop Black ( row, col ))

                4 ->
                    Just (Piece King Black ( row, col ))

                5 ->
                    Just (Piece Queen Black ( row, col ))

                6 ->
                    Just (Piece Bishop Black ( row, col ))

                7 ->
                    Just (Piece Knight Black ( row, col ))

                8 ->
                    Just (Piece Rook Black ( row, col ))

                _ ->
                    Nothing

        _ ->
            Nothing


createPieceWithIndexTuple : Square -> Maybe ( Square, Piece )
createPieceWithIndexTuple square =
    let
        maybePiece : Maybe Piece
        maybePiece =
            createPieceFromInitSquare square
    in
    Maybe.map (Tuple.pair square) maybePiece


initBoardState : BoardState
initBoardState =
    List.range 1 8
        |> List.concatMap (\row -> List.map (Tuple.pair row) (List.range 1 8))
        |> List.map createPieceWithIndexTuple
        |> List.filterMap identity
        |> Dict.fromList


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = initBoardState
      , moveStack = []
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
                        , calculatePossibleMoves model.board model.moveStack True piece square
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
                newBoardState : BoardState
                newBoardState =
                    case move of
                        RegularMove moveDetails ->
                            doMove moveDetails model.board

                        EnPassantMove { moveDetails, squareToClear } ->
                            doMove moveDetails model.board |> Dict.remove squareToClear

                        CastlingMove { kingMove, rookMove } ->
                            doMove kingMove model.board |> doMove rookMove

                isOtherKing : Piece -> Bool
                isOtherKing { type_, color } =
                    case type_ of
                        King ->
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
                , moveStack = move :: model.moveStack
                , possibleMoves = []
                , selection = Nothing
                , turn = otherColor model.turn
                , victory = invertMap model.turn enemyKing
              }
            , Cmd.none
            )

        ClickedPlayAgainButton ->
            init ()


doMove : MoveDetails -> BoardState -> BoardState
doMove { piece, from, to } boardState =
    boardState
        |> Dict.remove from
        |> Dict.insert to piece


otherColor : Color -> Color
otherColor color =
    case color of
        White ->
            Black

        Black ->
            White


calculatePossibleMoves : BoardState -> List Move -> Bool -> Piece -> Square -> List Move
calculatePossibleMoves boardState moveStack checkKingMoves ({ type_, color } as piece) (( row, col ) as square) =
    let
        regularMoves : MoveRestriction -> List (Square -> Square) -> List Move
        regularMoves moveRestriction moveFns =
            List.foldl
                (\moveFn acc ->
                    calculateMoveDestinationsFromSquare
                        boardState
                        square
                        color
                        moveRestriction
                        moveFn
                        ++ acc
                )
                []
                moveFns
                |> List.map (MoveDetails piece square >> RegularMove)
    in
    case type_ of
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
            calculatePawnMoves boardState moveStack color direction square piece

        Rook ->
            regularMoves
                UnlimitedMoves
                straightMoves

        Knight ->
            regularMoves
                SingleMove
                [ \( row_, col_ ) -> ( row_ + 2, col_ + 1 )
                , \( row_, col_ ) -> ( row_ + 1, col_ + 2 )
                , \( row_, col_ ) -> ( row_ - 1, col_ + 2 )
                , \( row_, col_ ) -> ( row_ - 2, col_ + 1 )
                , \( row_, col_ ) -> ( row_ - 2, col_ - 1 )
                , \( row_, col_ ) -> ( row_ - 1, col_ - 2 )
                , \( row_, col_ ) -> ( row_ + 1, col_ - 2 )
                , \( row_, col_ ) -> ( row_ + 2, col_ - 1 )
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
                castling : CastlingDirection -> Maybe Move
                castling direction =
                    let
                        rookToMove : Maybe Piece
                        rookToMove =
                            case ( color, direction ) of
                                ( White, Kingside ) ->
                                    Dict.get ( 1, 8 ) boardState

                                ( White, Queenside ) ->
                                    Dict.get ( 1, 1 ) boardState

                                ( Black, Kingside ) ->
                                    Dict.get ( 8, 1 ) boardState

                                ( Black, Queenside ) ->
                                    Dict.get ( 8, 8 ) boardState

                        horizontalSquareFromKing : Int -> Square
                        horizontalSquareFromKing n =
                            case ( color, direction ) of
                                ( White, Kingside ) ->
                                    ( row, col + n )

                                ( White, Queenside ) ->
                                    ( row, col - n )

                                ( Black, Kingside ) ->
                                    ( row, col - n )

                                ( Black, Queenside ) ->
                                    ( row, col + n )

                        kingDestination : Square
                        kingDestination =
                            horizontalSquareFromKing 2

                        rookDestination : Square
                        rookDestination =
                            horizontalSquareFromKing 1
                    in
                    case rookToMove of
                        Just rook ->
                            if
                                hasMovedTimes 0 moveStack piece
                                    && hasMovedTimes 0 moveStack rook
                            then
                                let
                                    rookCol : Int
                                    rookCol =
                                        Tuple.second rook.startSquare

                                    noPiecesBetween : Bool
                                    noPiecesBetween =
                                        List.range (min col rookCol + 1) (max col rookCol - 1)
                                            |> List.map (Tuple.pair row)
                                            |> List.filterMap (\square_ -> Dict.get square_ boardState)
                                            |> List.length
                                            |> (==) 0
                                in
                                if noPiecesBetween then
                                    Just
                                        (CastlingMove
                                            { kingMove = MoveDetails piece square kingDestination
                                            , rookMove = MoveDetails rook rook.startSquare rookDestination
                                            }
                                        )

                                else
                                    Nothing

                            else
                                Nothing

                        Nothing ->
                            Nothing

                moves : List Move
                moves =
                    regularMoves
                        SingleMove
                        (straightMoves ++ diagonalMoves)
                        ++ List.filterMap identity [ castling Kingside, castling Queenside ]
            in
            if checkKingMoves then
                List.filter (isMoveSafe boardState moveStack color) moves

            else
                moves


isMoveSafe : BoardState -> List Move -> Color -> Move -> Bool
isMoveSafe boardState moveStack color move =
    let
        dangerousMove moveDetails =
            List.member
                move
                (calculateAllMovesForColor
                    (doMove moveDetails boardState)
                    moveStack
                    (otherColor color)
                )
    in
    case move of
        RegularMove moveDetails ->
            not (dangerousMove moveDetails)

        EnPassantMove { moveDetails } ->
            not (dangerousMove moveDetails)

        CastlingMove { kingMove, rookMove } ->
            -- check if King is in check
            not (dangerousMove (MoveDetails kingMove.piece kingMove.from kingMove.from))
                -- check if any squares king will pass through are being attacked
                && not (dangerousMove kingMove)
                && not (dangerousMove rookMove)


calculateAllMovesForColor : BoardState -> List Move -> Color -> List Move
calculateAllMovesForColor boardState moveStack color =
    let
        maybePair : Square -> Maybe ( Square, Piece )
        maybePair key =
            Dict.get key boardState |> Maybe.map (Tuple.pair key)
    in
    Dict.keys boardState
        |> List.filterMap maybePair
        |> List.filter (Tuple.second >> .color >> (==) color)
        |> List.concatMap
            (\tuple ->
                calculatePossibleMoves
                    boardState
                    moveStack
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

                inhabitant : Maybe Piece
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


hasMovedTimes : Int -> List Move -> Piece -> Bool
hasMovedTimes times moveStack piece_ =
    let
        pieces : Move -> List Piece
        pieces move =
            case move of
                RegularMove { piece } ->
                    [ piece ]

                EnPassantMove { moveDetails } ->
                    [ moveDetails.piece ]

                CastlingMove { kingMove, rookMove } ->
                    [ kingMove.piece, rookMove.piece ]
    in
    List.concatMap pieces moveStack
        |> List.filter ((==) piece_)
        |> List.length
        |> (==) times


calculatePawnMoves : BoardState -> List Move -> Color -> (Int -> Int -> Int) -> Square -> Piece -> List Move
calculatePawnMoves boardState moveStack playerColor advanceDirection (( row, col ) as currentSquare) piece =
    let
        advanceOne : Square -> Square
        advanceOne ( row_, col_ ) =
            ( advanceDirection row_ 1, col_ )

        moveDetails : Square -> Maybe MoveDetails
        moveDetails destination =
            Just
                { piece = piece
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
                        Maybe.map RegularMove (moveDetails (advanceOne currentSquare))

                    else
                        Nothing
            in
            if hasMovedTimes 0 moveStack piece && moveOne /= Nothing then
                [ Maybe.map RegularMove (moveDetails ( advanceDirection row 2, col )), moveOne ]

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
                                (RegularMove
                                    { piece = piece
                                    , from = currentSquare
                                    , to = destination
                                    }
                                )
                            )

                -- En passant requirements
                -- 1. The capturing pawn must have advanced exactly three ranks to perform this move.
                -- 2. The captured pawn must have moved two squares in one move, landing right next to the capturing pawn.
                -- 3. The en passant capture must be performed on the turn immediately after the pawn being captured moves.
                --    If the player does not capture en passant on that turn, they no longer can do it later.
                maybeEnPassantMove : (Int -> Int -> Int) -> Maybe Move
                maybeEnPassantMove horizontalDirection =
                    let
                        previousMove : Maybe Move
                        previousMove =
                            List.head moveStack
                    in
                    case previousMove of
                        Just (RegularMove moveDetails_) ->
                            if
                                (piece.startSquare |> advanceOne |> advanceOne |> advanceOne)
                                    == currentSquare
                                    && moveDetails_.piece.type_
                                    == Pawn
                                    && hasMovedTimes 1 moveStack moveDetails_.piece
                                    && moveDetails_.to
                                    == ( row, horizontalDirection col 1 )
                            then
                                Just
                                    (EnPassantMove
                                        { moveDetails =
                                            { piece = piece
                                            , from = currentSquare
                                            , to = ( advanceDirection row 1, horizontalDirection col 1 )
                                            }
                                        , squareToClear = moveDetails_.to
                                        }
                                    )

                            else
                                Nothing

                        _ ->
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
        (List.range 1 8 |> List.map (Tuple.pair rowIndex >> viewCell model))


viewCell : Model -> ( Int, Int ) -> Html Msg
viewCell model (( rowIndex, colIndex ) as square_) =
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
            Dict.get square_ model.board

        destinationSquare : Move -> Square
        destinationSquare move =
            case move of
                RegularMove { to } ->
                    to

                EnPassantMove { moveDetails } ->
                    moveDetails.to

                CastlingMove { kingMove } ->
                    kingMove.to

        maybeMove : Maybe Move
        maybeMove =
            model.possibleMoves
                |> List.filter (destinationSquare >> (==) square_)
                |> List.head

        isSelectedField : Bool
        isSelectedField =
            Maybe.map .square model.selection == Just square_

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
                        Just ({ type_, color } as piece) ->
                            if color == model.turn then
                                [ onClick (ClickedFieldWithPiece piece square_) ]

                            else
                                []

                        Nothing ->
                            []
    in
    Html.div
        (styles ++ eventHandlers)
        (viewPieceAndMove maybePiece (maybeMove /= Nothing))


viewPieceAndMove : Maybe Piece -> Bool -> List (Html msg)
viewPieceAndMove maybePieceDetails isMove =
    case ( maybePieceDetails, isMove ) of
        ( Just { type_ }, True ) ->
            [ Html.text (pieceIcon type_)
            , Html.div [ Attr.class "scale-150 absolute w-full h-full text-red-500 flex justify-center items-center" ] [ Html.text "X" ]
            ]

        ( Just { type_ }, False ) ->
            [ Html.text (pieceIcon type_) ]

        ( Nothing, True ) ->
            [ Html.text "o" ]

        ( Nothing, False ) ->
            [ Html.text "" ]


pieceIcon : PieceType -> String
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
