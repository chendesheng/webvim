module View.Guide exposing (renderGuide)

import Font exposing (FontInfo)
import Helper.Helper exposing (ch, px)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Internal.TextBuffer as B
import Update.Cursor exposing (cursorPoint)
import View.Gutter exposing (gutterWidth)


renderGuide : FontInfo -> B.TextBuffer -> Int -> Int -> Int -> Maybe Position -> Html msg
renderGuide fontInfo lines lineHeight scrollTop topOffsetPx cursor =
    div [ class "guides" ]
        (case cursor of
            Just ( y, x ) ->
                [ lazy renderLineGuide (lineHeight * (y - scrollTop) - topOffsetPx)
                , lazy4 renderColumnGuide fontInfo lines y x
                , lazy renderRuler lines
                ]

            _ ->
                [ lazy renderRuler lines ]
        )


renderRuler : B.TextBuffer -> Html msg
renderRuler lines =
    div
        [ class "ruler"
        , style "transform" ("translateX(" ++ ch (gutterWidth lines) ++ ")")
        ]
        []


renderLineGuide : Int -> Html msg
renderLineGuide y =
    div
        [ class "guide line-guide"
        , style "top" <| px y
        ]
        []


renderColumnGuide : FontInfo -> B.TextBuffer -> Int -> Int -> Html msg
renderColumnGuide fontInfo lines y x =
    let
        ( ( _, x1 ), ( _, x2 ) ) =
            cursorPoint fontInfo lines y x
    in
    div
        [ class "guide column-guide"
        , style "transform" ("translateX(" ++ ch (gutterWidth lines) ++ ")")
        , style "left" <| px x1
        , style "width" <| px (x2 - x1)
        ]
        []
