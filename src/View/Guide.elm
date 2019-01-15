module View.Guide exposing (renderColumnGuide, renderLineGuide)

import Helper.Helper exposing (px, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Internal.TextBuffer as B
import Model exposing (FontInfo)
import Update.Cursor exposing (cursorPoint)


renderLineGuide : Maybe Position -> Html msg
renderLineGuide cursor =
    case cursor of
        Just ( y, x ) ->
            lazy renderLineGuideInner y

        _ ->
            text ""


renderLineGuideInner : Int -> Html msg
renderLineGuideInner y =
    div
        [ class "guide line-guide"
        , style "top" <| rem y
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
