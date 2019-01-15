module View.Gutter exposing (renderGutters)

import Helper.Helper exposing (ch, rem)
import Helper.KeyEvent exposing (decodeKeyboardEvent)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Lazy exposing (..)
import Internal.TextBuffer as B
import Model exposing (FontInfo)


renderGutters :
    List Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List (Attribute msg)
    -> Html msg
renderGutters viewLines totalLines lineHeight relativeZeroLine scrollTop1 topOffsetPx height scrollingCss =
    let
        gutterWidth =
            totalLines |> String.fromInt |> String.length

        relativeGutterWidth =
            4
    in
    div
        [ class "gutters"
        , style "width" <| ch (gutterWidth + relativeGutterWidth + 1)
        ]
        [ renderGutter
            scrollingCss
            gutterWidth
            relativeZeroLine
            totalLines
            viewLines
        , lazy5 renderRelativeGutter
            lineHeight
            topOffsetPx
            height
            (relativeZeroLine - scrollTop1)
            (totalLines - scrollTop1)
        ]


renderGutter :
    List (Attribute msg)
    -> Int
    -> Int
    -> Int
    -> List Int
    -> Html msg
renderGutter scrollingCss totalWidth highlightLine totalLines viewLines =
    div
        ([ class "gutter absolute-gutter"
         , style "width" <| ch <| totalWidth + 1
         ]
            ++ scrollingCss
        )
        [ lazy3 renderGutterInner totalLines highlightLine viewLines ]


renderGutterInner : Int -> Int -> List Int -> Html msg
renderGutterInner totalLines highlightLine viewLines =
    div
        []
        (List.map
            (\lineNumber ->
                if lineNumber < totalLines then
                    div
                        [ classList
                            [ ( "line-number-highlight"
                              , highlightLine == lineNumber
                              )
                            , ( "line-number", True )
                            ]
                        , style "top" <| rem lineNumber
                        ]
                        [ text (String.fromInt (lineNumber + 1)) ]

                else
                    div [] []
            )
            viewLines
        )


renderAllRelativeNumbers : Int -> Int -> Html msg
renderAllRelativeNumbers low high =
    div
        []
        ((List.range 1 low
            |> List.reverse
            |> List.map
                (\i ->
                    div [ class "line-number" ]
                        [ text <| String.fromInt i ]
                )
         )
            ++ (List.range 0 (high - 1)
                    |> List.map
                        (\i ->
                            div [ class "line-number" ]
                                [ text <| String.fromInt i ]
                        )
               )
        )


renderRelativeGutter : Int -> Int -> Int -> Int -> Int -> Html msg
renderRelativeGutter lineHeight topOffsetPx height zeroLine maxLine =
    div
        [ class "gutter"
        , class "relative-gutter"
        , style "top" <|
            String.fromInt ((zeroLine - height) * lineHeight - topOffsetPx)
                ++ "px"
        ]
        [ lazy2 renderAllRelativeNumbers
            height
            (Basics.min (maxLine - zeroLine) height)
        ]
