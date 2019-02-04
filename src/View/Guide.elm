module View.Guide exposing (renderColumnGuide, renderLineGuide)

import Font exposing (FontInfo)
import Helper.Helper exposing (px)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Internal.TextBuffer as B
import Update.Cursor exposing (cursorPoint)


renderLineGuide : Int -> Int -> Int -> Maybe Position -> Html msg
renderLineGuide lineHeight scrollTop topOffsetPx cursor =
    case cursor of
        Just ( y, x ) ->
            lazy renderLineGuideInner (lineHeight * (y - scrollTop) - topOffsetPx)

        _ ->
            text ""


renderLineGuideInner : Int -> Html msg
renderLineGuideInner y =
    div
        [ class "guide line-guide"
        , style "top" <| px y
        ]
        []


renderColumnGuide : FontInfo -> B.TextBuffer -> Maybe Position -> Html msg
renderColumnGuide fontInfo lines cursor =
    case cursor of
        Just ( y, x ) ->
            lazy4 renderCursorColumnInner fontInfo lines y x

        _ ->
            text ""


renderCursorColumnInner : FontInfo -> B.TextBuffer -> Int -> Int -> Html msg
renderCursorColumnInner fontInfo lines y x =
    let
        ( ( _, x1 ), ( _, x2 ) ) =
            cursorPoint fontInfo lines y x
    in
    div
        [ class "guide column-guide"
        , style "left" <| px x1
        , style "width" <| px (x2 - x1)
        ]
        []
