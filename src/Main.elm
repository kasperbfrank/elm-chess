module Main exposing (Msg(..), main, update, view)

import Arithmetic as Math
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type Piece
    = Pawn


type alias Field =
    { position : Position, piece : Maybe Piece }


type alias Model =
    { pieces : Dict String Piece
    , selection : Maybe Selection
    , possibleMoves : List Position
    }


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


positionFromIndex : String -> Position
positionFromIndex indexStr =
    let
        ints : List Int
        ints =
            String.toList indexStr
                |> List.map (String.fromChar >> (String.toInt >> Maybe.withDefault -1))

        row : Int
        row =
            Maybe.withDefault 0 (List.head ints)

        col : Int
        col =
            Maybe.withDefault 0 (List.head (List.reverse ints))
    in
    ( row, col )


createPieceFromInitPosition : Position -> Maybe Piece
createPieceFromInitPosition position =
    case position of
        ( 2, _ ) ->
            Just Pawn

        ( 7, _ ) ->
            Just Pawn

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


initDict : Dict String Piece
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
    ( { pieces = initDict, selection = Nothing, possibleMoves = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFieldWithPiece piece position ->
            let
                ( selectedPiece, possibleMoves ) =
                    if Just piece == Maybe.map .piece model.selection then
                        ( Nothing, [] )

                    else
                        ( Just (Selection piece position), calculatePossibleMoves piece position )
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
              }
            , Cmd.none
            )


calculatePossibleMoves : Piece -> Position -> List Position
calculatePossibleMoves selectedPiece ( x, y ) =
    case selectedPiece of
        Pawn ->
            [ ( x + 1, y ) ]


view : Model -> Html Msg
view model =
    Html.main_ [ Attr.class "w-full h-full flex justify-center items-center select-none" ]
        [ Html.section
            []
            (List.reverse (List.range 1 8) |> List.map (viewRow2 model))
        ]


viewRow2 : Model -> Int -> Html Msg
viewRow2 model rowIndex =
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
            [ Attr.class "h-20 w-20 transition-all flex justify-center items-center text-3xl font-bold"
            , Attr.class
                (if isSelectedField then
                    "border-4 border-emerald-400 text-emerald-400"

                 else
                    "text-black"
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
                        [ onClick (ClickedFieldWithPiece piece ( rowIndex, colIndex )) ]

                    Nothing ->
                        []
    in
    Html.div
        (styles ++ eventHandlers)
        [ viewPieceAndMove maybePiece isMove ]


viewPieceAndMove : Maybe Piece -> Bool -> Html msg
viewPieceAndMove maybePiece isMove =
    let
        pieceText : String
        pieceText =
            case maybePiece of
                Just Pawn ->
                    "P"

                Nothing ->
                    ""

        moveText : String
        moveText =
            if isMove then
                "O"

            else
                ""
    in
    Html.text (pieceText ++ moveText)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
