module Main exposing (Msg(..), calculatePawnMovesFromPosition, main, update, view)

import Arithmetic as Math
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type Piece
    = Pawn Color
    | Rook Color
    | Knight Color
    | Bishop Color
    | Queen Color
    | King Color


type Color
    = White
    | Black


type MoveCount
    = SingleMove
    | UnlimitedMoves


type alias Field =
    { position : Position, piece : Maybe Piece }


type alias Model =
    { pieces : PiecesDict
    , selection : Maybe Selection
    , possibleMoves : List Position
    , turn : Color
    }


type alias PiecesDict =
    Dict String Piece


type Msg
    = ClickedFieldWithPiece Piece Position
    | ClickedFieldWithMove MovePieceEvent


type alias Selection =
    { piece : Piece, position : Position }


type alias MovePieceEvent =
    { piece : Piece, from : Position, to : Position }


type alias Position =
    ( Int, Int )


positionToIndex : Position -> String
positionToIndex ( row, col ) =
    String.fromInt row ++ String.fromInt col


createPieceFromInitPosition : Position -> Maybe Piece
createPieceFromInitPosition ( row, col ) =
    case row of
        1 ->
            case col of
                1 ->
                    Just (Rook White)

                2 ->
                    Just (Knight White)

                3 ->
                    Just (Bishop White)

                4 ->
                    Just (Queen White)

                5 ->
                    Just (King White)

                6 ->
                    Just (Bishop White)

                7 ->
                    Just (Knight White)

                8 ->
                    Just (Rook White)

                _ ->
                    Nothing

        2 ->
            Just (Pawn White)

        7 ->
            Just (Pawn Black)

        8 ->
            case col of
                1 ->
                    Just (Rook Black)

                2 ->
                    Just (Knight Black)

                3 ->
                    Just (Bishop Black)

                4 ->
                    Just (King Black)

                5 ->
                    Just (Queen Black)

                6 ->
                    Just (Bishop Black)

                7 ->
                    Just (Knight Black)

                8 ->
                    Just (Rook Black)

                _ ->
                    Nothing

        _ ->
            Nothing


createPieceWithIndexTuple : Position -> Maybe ( String, Piece )
createPieceWithIndexTuple position =
    let
        maybePiece : Maybe Piece
        maybePiece =
            createPieceFromInitPosition position
    in
    Maybe.map (Tuple.pair (positionToIndex position)) maybePiece


initDict : PiecesDict
initDict =
    List.range 1 8
        |> List.map (\n -> ( n, List.range 1 8 ))
        |> List.concatMap
            (\( rowIndex, colIndexes ) ->
                colIndexes
                    |> List.map (\colIndex -> ( rowIndex, colIndex ))
            )
        |> List.map createPieceWithIndexTuple
        |> List.filterMap identity
        |> Dict.fromList


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pieces = initDict
      , selection = Nothing
      , possibleMoves = []
      , turn = White
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
                        , calculatePossibleMoves model.pieces piece position
                        )
            in
            ( { model
                | selection = selectedPiece
                , possibleMoves = possibleMoves
              }
            , Cmd.none
            )

        ClickedFieldWithMove { piece, from, to } ->
            ( { model
                | pieces =
                    Dict.remove (positionToIndex from) model.pieces
                        |> Dict.insert (positionToIndex to) piece
                , possibleMoves = []
                , selection = Nothing
                , turn = otherColor model.turn
              }
            , Cmd.none
            )


otherColor : Color -> Color
otherColor color =
    case color of
        White ->
            Black

        Black ->
            White


calculatePossibleMoves : PiecesDict -> Piece -> Position -> List Position
calculatePossibleMoves pieces selectedPiece position =
    let
        moves : Color -> MoveCount -> (Position -> Position) -> List Position
        moves =
            calculateMoves pieces position

        allMoves : Color -> MoveCount -> List (Position -> Position) -> List Position
        allMoves color moveCount moveFns =
            List.foldl (\moveFn acc -> moves color moveCount moveFn ++ acc) [] moveFns
    in
    case selectedPiece of
        Pawn color ->
            let
                direction : Int -> Int -> Int
                direction =
                    case color of
                        White ->
                            (+)

                        Black ->
                            (-)
            in
            calculatePawnMovesFromPosition pieces color direction position

        Rook color ->
            -- move unlimited units in step
            allMoves
                color
                UnlimitedMoves
                [ \( row, col ) -> ( row + 1, col )
                , \( row, col ) -> ( row - 1, col )
                , \( row, col ) -> ( row, col + 1 )
                , \( row, col ) -> ( row, col - 1 )
                ]

        Knight color ->
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

        Bishop color ->
            allMoves
                color
                UnlimitedMoves
                [ \( row, col ) -> ( row + 1, col + 1 )
                , \( row, col ) -> ( row + 1, col - 1 )
                , \( row, col ) -> ( row - 1, col - 1 )
                , \( row, col ) -> ( row - 1, col + 1 )
                ]

        Queen color ->
            allMoves
                color
                UnlimitedMoves
                [ \( row, col ) -> ( row + 1, col + 1 )
                , \( row, col ) -> ( row + 1, col - 1 )
                , \( row, col ) -> ( row - 1, col - 1 )
                , \( row, col ) -> ( row - 1, col + 1 )
                , \( row, col ) -> ( row + 1, col )
                , \( row, col ) -> ( row - 1, col )
                , \( row, col ) -> ( row, col + 1 )
                , \( row, col ) -> ( row, col - 1 )
                ]

        King color ->
            allMoves
                color
                SingleMove
                [ \( row, col ) -> ( row + 1, col + 1 )
                , \( row, col ) -> ( row + 1, col - 1 )
                , \( row, col ) -> ( row - 1, col - 1 )
                , \( row, col ) -> ( row - 1, col + 1 )
                , \( row, col ) -> ( row + 1, col )
                , \( row, col ) -> ( row - 1, col )
                , \( row, col ) -> ( row, col + 1 )
                , \( row, col ) -> ( row, col - 1 )
                ]



--List.foldl (\moveFn acc -> calculateMoves SingleMove pieces color position moveFn ++ acc) [] moveFns


calculateMoves : PiecesDict -> Position -> Color -> MoveCount -> (Position -> Position) -> List Position
calculateMoves pieces startPosition playerColor moveCount tryMove =
    let
        move : List Position -> Position -> List Position
        move acc from =
            let
                to : Position
                to =
                    tryMove from

                inhabitant : Maybe Piece
                inhabitant =
                    Dict.get (positionToIndex to) pieces
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


calculatePawnMovesFromPosition : PiecesDict -> Color -> (Int -> Int -> Int) -> Position -> List Position
calculatePawnMovesFromPosition pieces playerColor direction ( row, col ) =
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
                    if Dict.get (positionToIndex oneForward) pieces == Nothing then
                        Just oneForward

                    else
                        Nothing

                canMoveTwo : Bool
                canMoveTwo =
                    case playerColor of
                        White ->
                            row == 2

                        Black ->
                            row == 7
            in
            if canMoveTwo && moveOne /= Nothing then
                [ Just ( direction row 2, col ), moveOne ]

            else
                [ moveOne ]

        destroyMoves : List (Maybe ( Int, Int ))
        destroyMoves =
            -- Pawn can destroy another unit by moving one forward diagonally
            let
                maybeDestroyMove : Position -> Maybe Position
                maybeDestroyMove move =
                    Dict.get (positionToIndex move) pieces
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
        [ Html.section
            [ Attr.class "border-4 border-slate-800" ]
            (List.reverse (List.range 1 8) |> List.map (viewRow model))
        ]


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
            Dict.get (positionToIndex ( rowIndex, colIndex )) model.pieces

        isMove : Bool
        isMove =
            List.any ((==) ( rowIndex, colIndex )) model.possibleMoves

        isSelectedField =
            Maybe.map .position model.selection == Just ( rowIndex, colIndex )

        styles : List (Attribute msg)
        styles =
            [ Attr.class "h-20 w-20 flex justify-center items-center text-3xl font-bold"
            , Attr.class
                (if isSelectedField then
                    "transition-all border-4 border-cyan-400 text-cyan-400"

                 else if isMove then
                    "text-emerald-400"

                 else
                    case maybePiece of
                        Just piece ->
                            getColor piece |> colorToString

                        Nothing ->
                            ""
                )
            , cellClass
            ]

        eventHandlers : List (Attribute Msg)
        eventHandlers =
            if isMove then
                case model.selection of
                    Just { piece, position } ->
                        [ onClick (ClickedFieldWithMove (MovePieceEvent piece position ( rowIndex, colIndex ))) ]

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
        [ viewPieceAndMove maybePiece isMove ]


getColor : Piece -> Color
getColor piece =
    case piece of
        Pawn color ->
            color

        Rook color ->
            color

        Knight color ->
            color

        Bishop color ->
            color

        Queen color ->
            color

        King color ->
            color


viewPieceAndMove : Maybe Piece -> Bool -> Html msg
viewPieceAndMove maybePiece isMove =
    let
        pieceText : String
        pieceText =
            case maybePiece of
                Just piece ->
                    pieceIcon piece

                Nothing ->
                    ""

        moveText : String
        moveText =
            if isMove then
                "o"

            else
                ""
    in
    Html.text (pieceText ++ moveText)


pieceIcon : Piece -> String
pieceIcon piece =
    -- todo: consider actually using color here for color of "Icon"
    case piece of
        Pawn _ ->
            "P"

        Rook _ ->
            "R"

        Knight _ ->
            "H"

        Bishop _ ->
            "B"

        Queen _ ->
            "Q"

        King _ ->
            "K"


colorToString : Color -> String
colorToString color =
    case color of
        White ->
            "text-white"

        Black ->
            "text-black"


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
