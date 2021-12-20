module Main exposing (Msg(..), main, update, view)

import Arithmetic as Math
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type Piece
    = Pawn Position


type alias Field =
    { position : Position, piece : Maybe Piece }


type alias Model =
    { fields : List Field
    , pieces : Dict String Piece
    , selectedPiece : Maybe Piece
    , possibleMoves : List Position
    }


type Msg
    = ClickedField ClickFieldEvent


type alias ClickFieldEvent =
    { piece : Maybe Piece, isMove : Bool }


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
            Just (Pawn position)

        ( 7, _ ) ->
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


getPosition : Piece -> Position
getPosition piece =
    case piece of
        Pawn position_ ->
            position_


createTuple : Piece -> ( String, Piece )
createTuple piece =
    ( positionToIndex (getPosition piece), piece )


initDict : Dict String Piece
initDict =
    List.range 1 8
        |> List.map (\n -> ( n, List.range 1 8 ))
        |> List.concatMap
            (\( rowIndex, colIndexes ) ->
                colIndexes
                    |> List.map (\colIndex -> ( rowIndex, colIndex ))
            )
        |> List.map createPieceFromInitPosition
        |> List.filterMap identity
        |> List.map createTuple
        |> Dict.fromList


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fields = initFields, pieces = initDict, selectedPiece = Nothing, possibleMoves = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedField { piece, isMove } ->
            let
                selectedPiece =
                    if piece == model.selectedPiece then
                        Nothing

                    else
                        piece

                -- TODO: Move field if isMove is True
                updated =
                    if not isMove && isJust selectedPiece then
                        model.pieces

                    else
                        model.pieces |> Dict.remove (positionToIndex (getPosition piece))
            in
            ( { model
                | selectedPiece = selectedPiece
                , possibleMoves =
                    Maybe.map calculatePossibleMoves selectedPiece
                        |> Maybe.withDefault []
              }
            , Cmd.none
            )


calculatePossibleMoves_OLD : List Field -> Piece -> List Field
calculatePossibleMoves_OLD fields selectedPiece =
    case selectedPiece of
        Pawn position ->
            let
                ( x, y ) =
                    position

                allowedPositions : List ( Int, Int )
                allowedPositions =
                    [ ( x + 1, y ) ]
            in
            fields
                |> List.filter
                    (\field ->
                        List.any
                            (\allowedPosition -> allowedPosition == field.position)
                            allowedPositions
                    )


calculatePossibleMoves : Piece -> List Position
calculatePossibleMoves selectedPiece =
    let
        ( x, y ) =
            getPosition selectedPiece
    in
    [ ( x + 1, y ) ]


view : Model -> Html Msg
view model =
    Html.main_ [ Attr.class "w-full h-full flex justify-center items-center select-none" ]
        [ Html.table
            []
            (List.reverse (List.range 1 8) |> List.map (viewRow2 model))
        ]


viewRow2 : Model -> Int -> Html Msg
viewRow2 model rowIndex =
    Html.tr []
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

        -- TODO: Only assign click handlers to cells when there is an action.
        -- If move, assign click handler with move action.
        -- If to lift piece, assign click handler with select piece action
    in
    Html.td
        [ onClick (ClickedField (ClickFieldEvent maybePiece isMove))
        , Attr.class "h-20 w-20 transition-all text-center align-middle text-3xl font-bold"
        , Attr.class
            (if model.selectedPiece /= Nothing && maybePiece == model.selectedPiece then
                "border-4 border-emerald-400 text-emerald-400"

             else
                "border-0 border-transparent text-black"
            )
        , cellClass
        ]
        [ viewPieceAndMove maybePiece isMove ]



--view : Model -> Html Msg
--view model =
--    Html.main_ [ Attr.class "w-full h-full flex justify-center items-center select-none" ]
--        [ Html.section
--            [ Attr.class "border-4 border-indigo-200 cursor-pointer" ]
--            (List.reverse (List.range 0 7)
--                |> List.map (viewRow model)
--            )
--        ]
--viewRow : Model -> Int -> Html Msg
--viewRow model rowIndex =
--    model.fields
--        |> List.filter (.position >> Tuple.first >> (==) rowIndex)
--        |> List.map (viewField model.selectedPiece model.possibleMoves)
--        |> Html.div [ Attr.class "flex" ]
--viewField : Maybe Piece -> List Field -> Field -> Html Msg
--viewField selectedPiece possibleMoves field =
--    let
--        ( row, col ) =
--            field.position
--
--        isBlack : Bool
--        isBlack =
--            case ( Math.isEven row, Math.isEven col ) of
--                ( True, True ) ->
--                    True
--
--                ( False, False ) ->
--                    True
--
--                _ ->
--                    False
--
--        isMove : Bool
--        isMove =
--            List.any ((==) field) possibleMoves
--    in
--    Html.div
--        [ onClick (ClickedField (ClickFieldEvent field isMove))
--        , Attr.class
--            (if isBlack then
--                "bg-slate-700"
--
--             else
--                "bg-gray-200"
--            )
--        , Attr.class "h-20 w-20 transition-all flex justify-center items-center text-3xl font-bold"
--        , Attr.class
--            (if selectedPiece /= Nothing && field.piece == selectedPiece then
--                "border-4 border-emerald-400 text-emerald-400"
--
--             else
--                "border-0 border-transparent text-black"
--            )
--        ]
--        [ viewPieceAndMove field.piece isMove ]


viewPieceAndMove : Maybe Piece -> Bool -> Html msg
viewPieceAndMove maybePiece isMove =
    let
        pieceText : String
        pieceText =
            case maybePiece of
                Just (Pawn _) ->
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



-- Helpers


isJust : Maybe a -> Bool
isJust maybe =
    Maybe.map (\_ -> True) maybe |> Maybe.withDefault False
