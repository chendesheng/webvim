module View exposing (page)

import Array as Array exposing (Array)
import Bitwise as BW
import Browser exposing (Document)
import Dict exposing (Dict)
import Helper.Helper exposing (ch, percentStr, px, rem)
import Helper.KeyEvent exposing (decodeKeyboardEvent)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Keyed
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Internal.Syntax exposing (Syntax, Token)
import Internal.TextBuffer as B
import Internal.Window as Win
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Model exposing (..)
import String
import Update.Buffer as Buf
import Update.Cursor exposing (cursorPoint)
import Update.Message exposing (IMEMsg(..), Msg(..))
import Update.Range exposing (visualRegions)
import View.AutoComplete exposing (renderAutoComplete)
import View.Cursor exposing (renderCursor, renderMatchedCursor)
import View.Guide exposing (renderColumnGuide, renderLineGuide)
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
import View.Tooltip exposing (renderTip)
import Vim.AST exposing (VisualType(..))
import Vim.Helper exposing (parseKeys)


page : Global -> Document Msg
page global =
    case Win.getActiveView global.window of
        Just view ->
            case Dict.get view.bufId global.buffers of
                Just buf ->
                    { title = pageTitle buf
                    , body =
                        let
                            views =
                                Win.toList global.window

                            activeViewRect =
                                views
                                    |> List.filter .isActive
                                    |> List.head
                                    |> Maybe.map .rect
                                    |> Maybe.withDefault { y = 0, x = 0, width = 0, height = 0 }
                        in
                        [ div [ class "editor" ]
                            (renderBuffers global views
                                :: renderStatus buf global
                                :: renderAutoComplete activeViewRect view buf global
                            )
                        , renderStorage global
                        ]
                    }

                _ ->
                    { title = ""
                    , body = []
                    }

        _ ->
            { title = ""
            , body = []
            }


pageTitle : Buffer -> String
pageTitle buf =
    if Buf.isDirty buf then
        "• " ++ buf.name

    else
        buf.name


renderBuffers global views =
    div [ class "buffers" ]
        (List.map
            (\item ->
                let
                    view =
                        item.view

                    rect =
                        item.rect

                    isActive =
                        item.isActive
                in
                case Dict.get view.bufId global.buffers of
                    Just buf ->
                        renderBuffer item.path rect view buf isActive global

                    _ ->
                        div [] []
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
        { fontInfo, isSafari, ime, lint, lineHeight } =
            global

        { mode, name, continuation } =
            buf
    in
    renderStatusBar
        fontInfo
        { ime | isSafari = isSafari }
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


renderBuffer : Win.Path -> Win.Rect -> View -> Buffer -> Bool -> Global -> Html Msg
renderBuffer path rect view buf isActive global =
    let
        { mode, lines, syntax, continuation, history } =
            buf

        cursor =
            view.cursor

        { fontInfo, isSafari, ime, lint, lineHeight } =
            global

        ime1 =
            { ime | isSafari = isSafari }

        scrollTop =
            view.scrollTop

        height =
            view.size.height
                + 2

        topOffsetPx =
            remainderBy lineHeight view.scrollTopPx

        totalLines =
            B.count lines - 1

        maybeCursor =
            case mode of
                Ex _ ->
                    Nothing

                _ ->
                    Just cursor

        scrollTop1 =
            Buf.finalScrollTop view.size view buf

        highlights =
            incrementSearchRegion mode

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
                scrollTop1

        scrollLeftProp =
            -view.scrollLeftPx
                |> px
                |> style "left"

        mouseWheel path1 =
            Events.on "mousewheel" <|
                Decode.map2
                    (toFloat
                        >> round
                        >> MouseWheel path1
                    )
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
                    [ mouseWheel path ]

                else
                    []
               )
        )
        [ renderGutters view.lines
            totalLines
            lineHeight
            relativeZeroLine
            scrollTop1
            topOffsetPx
            height
            scrollingCss
        , div
            (class "lines-container" :: scrollLeftProp :: scrollingCss)
            (renderColumnGuide fontInfo lines maybeCursor
                :: renderLineGuide maybeCursor
                :: lazy5 renderVisual fontInfo scrollTop1 height mode lines
                :: renderHighlights fontInfo scrollTop1 lines highlights
                :: lazy5 renderLint buf.path fontInfo scrollTop1 lines lint.items
                :: lazy3 renderLines lines syntax view.lines
                :: div [ class "ruler" ] []
                :: renderCursor isActive fontInfo ime1 lines "" maybeCursor
                :: renderTip
                    view.size.width
                    lint.items
                    maybeCursor
                    showTip
                :: renderMatchedCursor
                    isActive
                    fontInfo
                    lines
                    mode
                    cursor
                    buf.view.matchedCursor
            )
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
    [ style "top" <|
        String.fromInt -(topOffsetPx + scrollTop * lineHeight)
            ++ "px"
    , style "height" <| rem (totalLines + height)
    ]
