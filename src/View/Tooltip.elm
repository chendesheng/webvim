module View.Tooltip exposing (renderTip)

import Helper.Helper exposing (ch, px, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Keyed
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Model exposing (LintError, RichText(..), TextWithStyle)


renderTip :
    Int
    -> List LintError
    -> Maybe Position
    -> Bool
    -> Html msg
renderTip width items maybeCursor showTip =
    if showTip then
        maybeCursor
            |> Maybe.map
                (\( y, x ) ->
                    lazy4 renderTipInner width y x items
                )
            |> Maybe.withDefault (text "")

    else
        text ""


renderTipInner : Int -> Int -> Int -> List LintError -> Html msg
renderTipInner width y_ x_ items =
    let
        cursor =
            ( y_, x_ )

        distanceFrom ( y, x ) { region } =
            let
                ( y1, x1 ) =
                    Tuple.first region
            in
            ( abs (y1 - y), abs (x1 - x) )

        renderDetails ( y, x ) overview details =
            div
                [ style "top" <| rem <| y + 1
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
