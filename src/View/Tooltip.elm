module View.Tooltip exposing (renderTooltip)

import Helper.Helper exposing (ch, px, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Keyed
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Internal.TextBuffer as B
import Internal.Window as Win
import Model
    exposing
        ( Buffer
        , FontInfo
        , Global
        , LintError
        , Mode(..)
        , RichText(..)
        , TextWithStyle
        , View
        )


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
                -- FIXME: usually source should not contains full-width characters
                -- if it does, the position here will be wrong
                lazy5 renderTooltipInner
                    global.lineHeight
                    view.scrollTop
                    topOffsetPx
                    view.cursor
                    global.lint.items

    else
        text ""


renderTooltipInner : Int -> Int -> Int -> Position -> List LintError -> Html msg
renderTooltipInner lineHeight scrollTop topOffsetPx cursor items =
    let
        distanceFrom ( y, x ) { region } =
            let
                ( y1, x1 ) =
                    Tuple.first region
            in
            ( abs (y1 - y), abs (x1 - x) )

        renderDetails ( y, x ) overview details =
            div
                [ style "top" <| px <| (Tuple.first cursor - scrollTop + 1) * lineHeight - topOffsetPx
                , style "left" <| ch x
                , class "tip"
                , case details of
                    RichText rt ->
                        class ""

                    PlainText s ->
                        style "max-width" "400px"
                ]
                [ div
                    [ class "tip-content" ]
                    (case details of
                        RichText rt ->
                            renderRichText rt

                        PlainText s ->
                            [ text s ]
                    )
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


renderRichText : List TextWithStyle -> List (Html msg)
renderRichText details =
    details
        |> List.map
            (\{ bold, underline, color, string } ->
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
            )
