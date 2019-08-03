module View.Tooltip exposing (renderTooltip)

import Font exposing (FontInfo, stringWidth)
import Helper.Helper exposing (ch, px, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Keyed
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Internal.TextBuffer as B
import Internal.Window as Win
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Model.View exposing (View)
import Update.Cursor exposing (cursorPoint)


renderTooltip :
    Global
    -> View
    -> Buffer
    -> Html msg
renderTooltip global view buf =
    -- lineHeight scrollTop width items showTip ( y, x ) mode =
    if global.showTip then
        case buf.mode of
            Insert _ ->
                text ""

            TempNormal ->
                text ""

            Ex _ ->
                text ""

            _ ->
                let
                    topOffsetPx =
                        remainderBy global.lineHeight view.scrollTopPx
                in
                lazy8 renderTooltipInner
                    global.fontInfo
                    buf.lines
                    global.size.width
                    global.lineHeight
                    view.scrollTop
                    topOffsetPx
                    view.cursor
                    global.lint.items

    else
        text ""


renderTooltipInner :
    FontInfo
    -> B.TextBuffer
    -> Int
    -> Int
    -> Int
    -> Int
    -> Position
    -> List LintError
    -> Html msg
renderTooltipInner fontInfo lines width lineHeight scrollTop topOffsetPx cursor items =
    let
        ( ( _, curx ), _ ) =
            cursorPoint fontInfo
                lines
                (Tuple.first cursor)
                (Tuple.second cursor)

        distanceFrom ( y, x ) { region } =
            let
                ( y1, x1 ) =
                    Tuple.first region
            in
            ( abs (y1 - y), abs (x1 - x) )

        renderDetails ( y, x ) overview details =
            div
                [ style "top" <|
                    px <|
                        (Tuple.first cursor - scrollTop + 1)
                            * lineHeight
                            - topOffsetPx
                , style "left" <| px curx
                , class "tip"
                ]
                [ case details of
                    [ PlainText s ] ->
                        div
                            [ class "tip-content"

                            -- FIXME
                            , (width - 100 - curx)
                                |> Basics.max 200
                                |> px
                                |> style "max-width"
                            ]
                            [ text (String.trim s) ]

                    _ ->
                        div [ class "tip-content" ]
                            (renderRichText details)
                ]
    in
    items
        |> List.filter
            (\item ->
                let
                    ( begin, end ) =
                        Maybe.withDefault
                            item.region
                            item.subRegion
                in
                begin <= cursor && cursor <= end
            )
        |> List.sortBy (distanceFrom cursor)
        |> List.head
        |> Maybe.map
            (\item ->
                renderDetails
                    cursor
                    item.overview
                    item.details
            )
        |> Maybe.withDefault (text "")


renderRichText : TextFragment -> List (Html msg)
renderRichText details =
    details
        |> List.map
            (\sp ->
                case sp of
                    RichText { bold, underline, color, string } ->
                        span
                            [ if bold then
                                style "font-weight" "700"

                              else
                                style "font-weight" "400"
                            , if underline then
                                style "text-decoration" "underline"

                              else
                                style "text-decoration" "none"
                            , case color of
                                Just c ->
                                    style "color" c

                                _ ->
                                    style "color" "inherit"
                            ]
                            [ text string ]

                    PlainText s ->
                        text s
            )
