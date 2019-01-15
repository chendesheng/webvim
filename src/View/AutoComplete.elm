module View.AutoComplete exposing (renderAutoComplete)

import Array as Array exposing (Array)
import Helper.Helper exposing (ch, percentStr)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Keyed
import Html.Lazy exposing (..)
import Internal.TextBuffer as B
import Internal.Window as Win
import Model exposing (AutoComplete, Buffer, Global, Mode(..), View)
import Update.Message exposing (Msg)


renderAutoComplete : Win.Rect -> View -> Buffer -> Global -> List (Html Msg)
renderAutoComplete rect view buf global =
    let
        lines =
            buf.lines

        totalLines =
            B.count lines - 1

        gutterWidth =
            totalLines |> String.fromInt |> String.length

        relativeGutterWidth =
            4

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
                                Nothing
                                lineHeight
                                topOffsetPx
                                True
                                scrollTop
                                (gutterWidth + relativeGutterWidth)
                            )
                        |> maybeToList

                _ ->
                    []

        Insert { autoComplete } ->
            case autoComplete of
                Just auto ->
                    [ renderAutoCompleteMenu
                        (Just ( rect.y, rect.x ))
                        lineHeight
                        topOffsetPx
                        False
                        scrollTop
                        (gutterWidth + relativeGutterWidth)
                        auto
                    ]

                _ ->
                    []

        _ ->
            []


renderAutoCompleteMenu :
    Maybe ( Float, Float )
    -> Int
    -> Int
    -> Bool
    -> Int
    -> Int
    -> AutoComplete
    -> Html msg
renderAutoCompleteMenu topLeft lineHeight topOffsetPx isEx viewScrollTop gutterWidth auto =
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
        ([ class "auto-complete"
         ]
            ++ (case topLeft of
                    Just ( top, left ) ->
                        [ style "top" (percentStr top)
                        , style "left" (percentStr left)
                        ]

                    Nothing ->
                        []
               )
            ++ (if isEx then
                    [ class "auto-complete-ex"
                    , style "left" <| ch (menuLeftOffset + 1)
                    ]

                else
                    [ style "left" <|
                        ch (x + gutterWidth)
                    , style "top" <|
                        String.fromInt
                            ((y + 1 - viewScrollTop)
                                * lineHeight
                                - topOffsetPx
                            )
                            ++ "px"
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
