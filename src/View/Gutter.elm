module View.Gutter exposing (gutterWidth, renderGutters)

import Font exposing (FontInfo)
import Helper.Helper exposing (ch, px, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Internal.TextBuffer as B


renderGutters :
    List Int
    -> B.TextBuffer
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> List (Attribute msg)
    -> Html msg
renderGutters viewLines lines lineHeight relativeZeroLine scrollTop topOffsetPx height scrollingCss =
    let
        totalLines =
            B.count lines - 1

        w =
            gutterWidth lines
    in
    div
        [ class "gutters"
        , style "width" <| ch w
        ]
        [ renderAbsoluteGutter
            scrollingCss
            (w - 5)
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
                let
                    top =
                        style "top" <| rem lineNumber
                in
                if lineNumber < totalLines then
                    renderLineNumber
                        [ top ]
                        (lineNumber + 1)

                else
                    div [ class "line-number", top, class "line-number-hole" ]
                        [ text "~" ]
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


gutterWidth : B.TextBuffer -> Int
gutterWidth lines =
    let
        n =
            B.count lines - 1
    in
    (n |> String.fromInt |> String.length) + 5
