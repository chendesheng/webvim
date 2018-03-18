module View exposing (..)

import Html exposing (..)
import Model exposing (..)
import Internal.TextBuffer as B
import Array
import Html.Attributes exposing (..)


view : Model -> Html msg
view { mode, cursor, view } =
    let
        ( y, x ) =
            cursor

        statusBar =
            getStatusBar mode

        statusText =
            statusBar.text
    in
        div [ class "buffer" ]
            [ div [ class "lines" ]
                (view.lines
                    |> B.mapLines
                        (\line ->
                            div [ class "line" ] [ text line ]
                        )
                    |> Array.toList
                )
            , div
                [ class "cursor"
                , style
                    [ ( "left", (toString x) ++ "ch" )
                    , ( "top", (toString y) ++ "em" )
                    ]
                ]
                []
            , div
                [ class "status"
                ]
                [ text statusText ]
            ]
