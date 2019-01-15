module View.AutoComplete exposing (renderAutoComplete, renderExAutoComplete)

import Array as Array exposing (Array)
import Helper.Helper exposing (ch, percentStr, px)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Keyed
import Html.Lazy exposing (..)
import Internal.TextBuffer as B
import Internal.Window as Win
import Model exposing (AutoComplete, Buffer, Global, Mode(..), View)
import Update.Message exposing (Msg)


renderAutoComplete : View -> Buffer -> Global -> List (Html Msg)
renderAutoComplete view buf global =
    let
        scrollTop =
            view.scrollTop

        lineHeight =
            global.lineHeight

        topOffsetPx =
            remainderBy lineHeight view.scrollTopPx
    in
    case buf.mode of
        Insert { autoComplete } ->
            case autoComplete of
                Just auto ->
                    [ renderAutoCompleteMenu
                        lineHeight
                        topOffsetPx
                        False
                        scrollTop
                        auto
                    ]

                _ ->
                    []

        _ ->
            []


renderExAutoComplete : View -> Buffer -> Global -> List (Html Msg)
renderExAutoComplete view buf global =
    let
        scrollTop =
            view.scrollTop

        lineHeight =
            global.lineHeight

        topOffsetPx =
            remainderBy lineHeight view.scrollTopPx
    in
    case buf.mode of
        Ex ex ->
            case ex.exbuf.mode of
                Insert { autoComplete } ->
                    autoComplete
                        |> Maybe.map
                            (renderAutoCompleteMenu
                                lineHeight
                                topOffsetPx
                                True
                                scrollTop
                            )
                        |> maybeToList

                _ ->
                    []

        _ ->
            []


renderAutoCompleteMenu :
    Int
    -> Int
    -> Bool
    -> Int
    -> AutoComplete
    -> Html msg
renderAutoCompleteMenu lineHeight topOffsetPx isEx viewScrollTop auto =
    let
        { matches, select, scrollTop, pos, menuLeftOffset } =
            auto

        index =
            select - scrollTop

        renderSpan s matched =
            if matched then
                span
                    [ class "matched" ]
                    [ text s ]

            else
                text s

        renderText s indexes i =
            let
                render i_ j matched =
                    renderSpan (String.slice i_ j s) matched
            in
            case indexes of
                j :: rest ->
                    if i < j then
                        render i j False
                            :: render j (j + 1) True
                            :: renderText s rest (j + 1)

                    else if i == j then
                        render j (j + 1) True
                            :: renderText s rest (j + 1)

                    else
                        -- shuld never happen
                        []

                _ ->
                    let
                        len =
                            String.length s
                    in
                    if i <= len - 1 then
                        [ render i len False ]

                    else
                        []

        ( y, x ) =
            pos
    in
    div
        ([ class "auto-complete" ]
            ++ (if isEx then
                    [ class "auto-complete-ex"
                    , style "left" <| ch (menuLeftOffset + 2)
                    ]

                else
                    [ style "left" <| ch x
                    , style "top" <|
                        px
                            ((y + 1 - viewScrollTop)
                                * lineHeight
                                - topOffsetPx
                            )
                    ]
               )
        )
        (List.indexedMap
            (\i m ->
                div
                    (if i == index then
                        [ class "selected" ]

                     else
                        []
                    )
                    (renderText m.text m.matches 0)
            )
            (matches
                |> Array.slice 0 -1
                |> Array.slice scrollTop (scrollTop + 15)
                |> Array.toList
            )
        )


maybeToList : Maybe a -> List a
maybeToList mb =
    case mb of
        Just x ->
            [ x ]

        _ ->
            []
