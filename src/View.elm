module View exposing (page)

import Browser exposing (Document)
import Dict
import Font exposing (charWidth)
import Helper.Helper exposing (percentStr, px, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Lazy exposing (..)
import Internal.TextBuffer as B
import Internal.Window as Win
import Json.Decode as Decode
import Json.Encode as Encode
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Frame as Frame
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Model.LoadBuffer exposing (..)
import Model.View exposing (emptyView)
import Update.Buffer as Buf
import Update.Message exposing (Msg(..))
import Url.Builder as Query exposing (toQuery)
import View.AutoComplete exposing (renderAutoComplete, renderExAutoComplete)
import View.Cursor exposing (renderCursor, renderMatchedCursor)
import View.Guide exposing (renderGuide)
import View.Gutter exposing (renderGutters)
import View.Lines
    exposing
        ( renderHighlights
        , renderLines
        , renderLint
        , renderVisual
        )
import View.StatusBar exposing (renderStatusBar)
import View.Storage exposing (renderStorage)
import View.Tooltip exposing (renderTooltip)
import Vim.AST exposing (VisualType(..))


page : Global -> Document Msg
page global =
    { title =
        getActiveBuffer global
            |> Maybe.map pageTitle
            |> Maybe.withDefault ""
    , body =
        [ lazy2 renderThemeCss global.service global.theme
        , div
            [ class "editor" ]
            (renderBuffers global (Win.toList global.window)
                :: renderActiveBufferParts global
            )
        , renderStorage global
        ]
    }


cssLink : String -> Html msg
cssLink href =
    node "link"
        [ property "rel" <| Encode.string "stylesheet"
        , property "href" <| Encode.string href
        ]
        []


renderThemeCss : String -> String -> Html msg
renderThemeCss service theme =
    cssLink
        (service
            ++ "/css"
            ++ toQuery [ Query.string "theme" theme ]
        )


pageTitle : Buffer -> String
pageTitle buf =
    let
        name =
            if String.isEmpty buf.name then
                "[no name]"

            else
                buf.name
    in
    if Buf.isDirty buf then
        "• " ++ name

    else
        name


renderActiveBufferParts : Global -> List (Html Msg)
renderActiveBufferParts global =
    global.window
        |> Win.getActiveFrame
        |> Maybe.andThen Frame.getActiveView
        |> Maybe.andThen
            (\view ->
                global.buffers
                    |> getBuffer view.bufId
                    |> Maybe.map
                        (\buf ->
                            renderStatus buf global
                                :: renderExAutoComplete view buf global
                        )
            )
        |> Maybe.withDefault []


renderBuffers global views =
    div [ class "buffers" ]
        (List.concatMap
            (\item ->
                let
                    view =
                        Frame.getActiveView item.frame
                            |> Maybe.withDefault emptyView

                    rect =
                        item.rect
                in
                case Dict.get view.bufId global.buffers of
                    Just (Loaded buf) ->
                        let
                            buf1 =
                                { buf
                                    | view = { view | isActive = item.isActive }
                                }
                        in
                        [ renderBuffer item.path rect buf1 global
                        , renderOverlay rect buf1 global
                        ]

                    Just (NotLoad buf) ->
                        -- this is a place holder for not loaded buffer
                        [ renderNotLoadBuffer rect
                        , renderOverlay rect buf global
                        ]

                    _ ->
                        []
            )
            views
            ++ renderBorders (Win.toBorders global.window)
        )


renderBorders : List Win.Rect -> List (Html msg)
renderBorders borders =
    List.map
        (\{ y, x, width, height } ->
            div
                [ classList
                    [ ( "border-hor", height == 0 )
                    , ( "border-ver", width == 0 )
                    ]
                , class "view-border"
                , style "top" (percentStr y)
                , style "left" (percentStr x)
                , style "width" (percentStr width)
                , style "height" (percentStr height)
                , title "todo"
                ]
                []
        )
        borders


renderStatus : Buffer -> Global -> Html Msg
renderStatus buf global =
    let
        { fontInfo, ime, lint, lineHeight } =
            global

        { mode, name, continuation } =
            buf
    in
    renderStatusBar
        fontInfo
        ime
        (Buf.isDirty buf)
        mode
        continuation
        lint.items
        name


isListenMouseWheel : Mode -> Bool -> BufferLint -> Bool
isListenMouseWheel mode showTip lint =
    case mode of
        Ex _ ->
            False

        Insert { autoComplete } ->
            case autoComplete of
                Just _ ->
                    False

                _ ->
                    not showTip || List.isEmpty lint.items

        _ ->
            not showTip || List.isEmpty lint.items


renderOverlay : Win.Rect -> Buffer -> Global -> Html Msg
renderOverlay rect buf global =
    let
        view =
            buf.view

        isActive =
            view.isActive

        totalLines =
            B.count buf.lines - 1

        gutterWidth =
            (5 + (totalLines |> String.fromInt |> String.length |> toFloat))
                * charWidth global.fontInfo '0'
    in
    div
        [ class "overlay"
        , style "top" <| percentStr rect.y
        , (toFloat global.size.width * rect.x + gutterWidth)
            |> round
            |> px
            |> style "left"
        ]
        (if isActive then
            renderTooltip global view buf :: renderAutoComplete view buf global

         else
            []
        )


renderNotLoadBuffer : Win.Rect -> Html msg
renderNotLoadBuffer rect =
    div
        [ class "buffer"
        , style "top" (percentStr rect.y)
        , style "left" (percentStr rect.x)
        , style "width" (percentStr rect.width)
        , style "height" (percentStr rect.height)
        ]
        []


renderBuffer : Win.Path -> Win.Rect -> Buffer -> Global -> Html Msg
renderBuffer path rect buf global =
    let
        { view, mode, lines, syntax, continuation, history } =
            buf

        { isActive, cursor } =
            view

        { fontInfo, ime, lint, lineHeight } =
            global

        -- size.height is number of "complete" lines
        -- first line & last may be "incomplete", so padding 2 lines
        height =
            view.size.height + 2

        topOffsetPx =
            view.scrollTopOffsetPx

        totalLines =
            B.count lines - 1

        maybeCursor =
            if isActive then
                case mode of
                    Ex _ ->
                        Nothing

                    _ ->
                        Just cursor

            else
                Just cursor

        scrollTop =
            Buf.finalScrollTop buf

        highlights =
            if isActive then
                incrementSearchRegion mode

            else
                Nothing

        relativeZeroLine =
            highlights
                |> Maybe.map (Tuple.first >> .begin)
                |> Maybe.withDefault cursor
                |> Tuple.first

        showTip =
            case buf.mode of
                Insert _ ->
                    False

                TempNormal ->
                    False

                _ ->
                    global.showTip

        scrollingCss =
            scrollingStyle
                lineHeight
                topOffsetPx
                totalLines
                height
                scrollTop

        scrollLeftProp =
            -view.scrollLeftPx
                |> px
                |> style "left"

        mouseClick path1 =
            Events.onClick <| MouseClick path1

        mouseWheel path1 =
            Events.on "mousewheel" <|
                Decode.map2
                    (toFloat >> round >> MouseWheel path1)
                    (Decode.at [ "deltaY" ] Decode.int)
                    (Decode.at [ "deltaX" ] Decode.int)
    in
    div
        ([ class "buffer"
         , style "top" (percentStr rect.y)
         , style "left" (percentStr rect.x)
         , style "width" (percentStr rect.width)
         , style "height" (percentStr rect.height)
         ]
            ++ (if isListenMouseWheel buf.mode showTip lint then
                    [ mouseClick path
                    , mouseWheel path
                    ]

                else
                    []
               )
        )
        [ renderGuide fontInfo lines lineHeight scrollTop topOffsetPx maybeCursor
        , renderGutters view.gutterLines
            lines
            lineHeight
            relativeZeroLine
            scrollTop
            topOffsetPx
            height
            scrollingCss
        , div [ class "content" ]
            [ div
                (class "scroll-content" :: scrollLeftProp :: scrollingCss)
                (lazy5 renderVisual fontInfo scrollTop height mode lines
                    :: renderHighlights fontInfo scrollTop lines highlights
                    :: lazy5 renderLint buf.path fontInfo scrollTop lines lint.items
                    :: lazy3 renderLines lines syntax view.lines
                    :: renderCursor isActive fontInfo ime lines "" maybeCursor
                    :: renderMatchedCursor
                        isActive
                        fontInfo
                        lines
                        mode
                        cursor
                        buf.view.matchedCursor
                )
            ]
        ]


incrementSearchRegion : Mode -> Maybe ( VisualMode, List VisualMode )
incrementSearchRegion mode =
    case mode of
        Ex { prefix, visual } ->
            case prefix of
                ExSearch { match, highlights } ->
                    case match of
                        Just ( begin, end ) ->
                            Just
                                ( { tipe = VisualChars
                                  , begin = begin
                                  , end = end
                                  }
                                , List.map
                                    (\( b, e ) ->
                                        { tipe = VisualChars
                                        , begin = b
                                        , end = e
                                        }
                                    )
                                    highlights
                                )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


scrollingStyle : Int -> Int -> Int -> Int -> Int -> List (Attribute msg)
scrollingStyle lineHeight topOffsetPx totalLines height scrollTop =
    [ style "top" <| px -(topOffsetPx + scrollTop * lineHeight)
    , style "height" <| rem (totalLines + height)
    ]
