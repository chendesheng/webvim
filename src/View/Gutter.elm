module View.Gutter exposing (renderGutters)

import Font exposing (FontInfo)
import Helper.Helper exposing (ch, px, rem)
import Helper.KeyEvent exposing (decodeKeyboardEvent)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Lazy exposing (..)
import Internal.TextBuffer as B


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
renderGutters viewLines totalLines lineHeight relativeZeroLine scrollTop topOffsetPx height scrollingCss =
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
        [ renderAbsoluteGutter
            scrollingCss
            gutterWidth
            relativeZeroLine
            totalLines
            viewLines
        , lazy5 renderRelativeGutter
            lineHeight
            topOffsetPx
            height
            (relativeZeroLine - scrollTop)
            (totalLines - scrollTop)
        ]


renderAbsoluteGutter :
    List (Attribute msg)
    -> Int
    -> Int
    -> Int
    -> List Int
    -> Html msg
renderAbsoluteGutter scrollingCss totalWidth highlightLine totalLines viewLines =
    div
        ([ class "gutter absolute-gutter"
         , style "width" <| ch <| totalWidth
         ]
            ++ scrollingCss
        )
        [ lazy2 renderAbsoluteGutterInner totalLines viewLines
        , lazy renderHighlightLine highlightLine
        ]


renderHighlightLine : Int -> Html msg
renderHighlightLine highlightLine =
    renderLineNumber
        [ class "line-number-highlight"
        , style "top" <| rem highlightLine
        ]
        (highlightLine + 1)


renderAbsoluteGutterInner : Int -> List Int -> Html msg
renderAbsoluteGutterInner totalLines viewLines =
    div
        []
        (List.map
            (\lineNumber ->
                if lineNumber < totalLines then
                    renderLineNumber
                        [ style "top" <| rem lineNumber ]
                        (lineNumber + 1)

                else
                    div [] []
            )
            viewLines
        )


renderLineNumber : List (Attribute msg) -> Int -> Html msg
renderLineNumber extra n =
    div ([ class "line-number" ] ++ extra)
        [ text <| String.fromInt n ]


renderRelativeNumbers : Int -> Int -> Html msg
renderRelativeNumbers low high =
    div
        []
        ((List.range 1 low
            |> List.reverse
            |> List.map (renderLineNumber [])
         )
            ++ (List.range 0 (high - 1)
                    |> List.map (renderLineNumber [])
               )
        )


renderRelativeGutter : Int -> Int -> Int -> Int -> Int -> Html msg
renderRelativeGutter lineHeight topOffsetPx height zeroLine maxLine =
    div
        [ class "gutter"
        , class "relative-gutter"
        , style "top" <|
            px ((zeroLine - height) * lineHeight - topOffsetPx)
        ]
        [ lazy2 renderRelativeNumbers
            height
            (Basics.min (maxLine - zeroLine) height)
        ]
