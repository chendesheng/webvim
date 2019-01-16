module View.Gutter exposing (renderGutters)

import Helper.Helper exposing (ch, px, rem)
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
            (relativeZeroLine - scrollTop1)
            (totalLines - scrollTop1)
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
        [ lazy3 renderAbsoluteGutterInner totalLines highlightLine viewLines ]


renderAbsoluteGutterInner : Int -> Int -> List Int -> Html msg
renderAbsoluteGutterInner totalLines highlightLine viewLines =
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


renderLineNumber : Int -> Html msg
renderLineNumber n =
    div [ class "line-number" ]
        [ text <| String.fromInt n ]


renderRelativeNumbers : Int -> Int -> Html msg
renderRelativeNumbers low high =
    div
        []
        ((List.range 1 low
            |> List.reverse
            |> List.map renderLineNumber
         )
            ++ (List.range 0 (high - 1)
                    |> List.map renderLineNumber
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
