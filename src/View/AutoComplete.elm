module View.AutoComplete exposing (renderAutoComplete, renderExAutoComplete)

import Helper.Helper exposing (ch, px)
import Html exposing (..)
import Html.Attributes exposing (..)
import Menu as Mu
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.View exposing (View)
import Update.Message exposing (Msg)


renderAutoComplete : View -> Buffer -> Global -> List (Html Msg)
renderAutoComplete view buf global =
    let
        scrollTop =
            view.scrollTop

        lineHeight =
            global.lineHeight

        topOffsetPx =
            view.scrollTopOffsetPx
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
            view.scrollTopOffsetPx
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
        { menu, pos, menuLeftOffset } =
            auto

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
                        -- should never happen
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
        (Mu.render
            (\selected m ->
                div
                    (if selected then
                        [ class "selected" ]

                     else
                        []
                    )
                    (renderText m.text m.matches 0)
            )
            menu
        )


maybeToList : Maybe a -> List a
maybeToList mb =
    case mb of
        Just x ->
            [ x ]

        _ ->
            []
