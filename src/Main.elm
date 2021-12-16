module Main exposing (Msg(..), main, update, view)

import Arithmetic as Math
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type Piece
    = Pawn Position


type alias Field =
    { position : Position, piece : Maybe Piece }


type alias Model =
    { fields : List Field, selectedPiece : Maybe Piece, possibleMoves : List Field }


type Msg
    = ClickedField Field


type alias Position =
    ( Int, Int )


createPieceFromInitPosition : Position -> Maybe Piece
createPieceFromInitPosition position =
    case position of
        ( 1, _ ) ->
            Just (Pawn position)

        ( 6, _ ) ->
            Just (Pawn position)

        _ ->
            Nothing


createPieceWithPosition : Position -> Field
createPieceWithPosition position =
    let
        piece : Maybe Piece
        piece =
            createPieceFromInitPosition position
    in
    { position = position, piece = piece }


initFields : List Field
initFields =
    List.range 0 7
        |> List.map (\n -> ( n, List.range 0 7 ))
        |> List.concatMap
            (\( rowIndex, colIndexes ) ->
                colIndexes
                    |> List.map (\colIndex -> ( rowIndex, colIndex ))
            )
        |> List.map createPieceWithPosition


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fields = initFields, selectedPiece = Nothing, possibleMoves = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedField field ->
            let
                _ =
                    Debug.log "field" field
            in
            ( { model | selectedPiece = field.piece }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.main_ [ Attr.class "w-full h-full flex justify-center items-center" ]
        [ Html.section
            [ Attr.class "border-4 border-indigo-200 cursor-pointer" ]
            (List.reverse (List.range 0 7)
                |> List.map (viewRow model.fields model.selectedPiece)
            )
        ]


viewRow : List Field -> Maybe Piece -> Int -> Html Msg
viewRow fields selectedPiece rowIndex =
    fields
        |> List.filter (.position >> Tuple.first >> (==) rowIndex)
        |> List.map (viewField selectedPiece)
        |> Html.div [ Attr.class "flex" ]


viewField : Maybe Piece -> Field -> Html Msg
viewField selectedPiece field =
    let
        ( row, col ) =
            field.position

        isBlack : Bool
        isBlack =
            case ( Math.isEven row, Math.isEven col ) of
                ( True, True ) ->
                    True

                ( False, False ) ->
                    True

                _ ->
                    False
    in
    Html.div
        [ onClick (ClickedField field)
        , Attr.class
            (if isBlack then
                "bg-slate-700"

             else
                "bg-gray-200"
            )
        , Attr.class "h-20 w-20 transition-all flex justify-center items-center text-3xl font-bold"
        , Attr.class
            (if selectedPiece /= Nothing && field.piece == selectedPiece then
                "border-4 border-emerald-400 text-emerald-400"

             else
                "border-0 border-transparent text-black"
            )
        ]
        [ pieceToText field.piece ]


pieceToText : Maybe Piece -> Html msg
pieceToText maybePiece =
    let
        text =
            case maybePiece of
                Just (Pawn _) ->
                    "P"

                Nothing ->
                    ""
    in
    Html.text text


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
