module Main exposing (..)

import Array
import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import TextBuffer as TB exposing (TextBuffer)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    List TextBuffer


init : Model
init =
    List.foldl
        (\action model ->
            case model of
                last :: _ ->
                    action last :: model

                _ ->
                    model
        )
        [ TB.fromString "0123456789" ]
        [ TB.moveCursorTo 3
        , TB.insert "abc\n" >> Tuple.first
        ]



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW
--pieceView : TextBuffer -> Piece -> Html msg
--pieceView t p =
--    case p.source of
--        PT.Original ->
--            span
--                [ style "background-color" "darkred"
--                , style "color" "white"
--                , style "white-space" "pre"
--                ]
--                [ text <| PT.getString t p ]
--
--        _ ->
--            span
--                [ style "background-color" "green"
--                , style "color" "white"
--                , style "white-space" "pre"
--                ]
--                [ text <| PT.getString t p ]
--pieceTablePiecesView : TextBuffer -> Html msg
--pieceTablePiecesView t =
--    t.pieces
--        |> Array.toList
--        |> List.map (pieceView t)
--        |> Html.p []


pieceTableLinesView : TextBuffer -> Html msg
pieceTableLinesView buf =
    TB.iterateLinesForward
        (TB.moveCursorToPoint ( 0, 0 ) buf)
        (\line lines ->
            Just <| div [] [ text line ] :: lines
        )
        []
        |> Html.p []


pieceTableView : TextBuffer -> Html msg
pieceTableView buf =
    div []
        --[ pieceTablePiecesView t
        [ pieceTableLinesView buf
        ]


view : Model -> Html Msg
view model =
    model
        |> List.map pieceTableView
        |> List.reverse
        |> div
            [ style "font-family" "monospace"
            , style "font-size" "20px"
            , style "padding" "20px"
            ]
