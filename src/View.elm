module View exposing (page)

import Array as Array exposing (Array)
import Bitwise as BW
import Browser exposing (Document)
import Dict exposing (Dict)
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
import Vim.AST exposing (VisualType(..))
import Vim.Helper exposing (parseKeys)


rem : Int -> String
rem n =
    String.fromInt n ++ "rem"


ch : Int -> String
ch n =
    String.fromInt n ++ "ch"


px : Int -> String
px n =
    String.fromInt n ++ "px"


translate : Int -> Int -> ( String, String )
translate x y =
    ( "transform"
    , "translate(" ++ ch x ++ ", " ++ rem y ++ ")"
    )


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

                            borders =
                                Win.toBorders global.window

                            activeViewRect =
                                views
                                    |> List.filter .isActive
                                    |> List.head
                                    |> Maybe.map .rect
                                    |> Maybe.withDefault { y = 0, x = 0, width = 0, height = 0 }
                        in
                        [ div [ class "editor" ]
                            [ div [ class "buffers" ]
                                (List.map
                                    (\item ->
                                        let
                                            view1 =
                                                item.view

                                            rect =
                                                item.rect

                                            isActive =
                                                item.isActive
                                        in
                                        case Dict.get view1.bufId global.buffers of
                                            Just buf1 ->
                                                renderBuffer item.path rect view1 buf1 isActive global

                                            _ ->
                                                div [] []
                                    )
                                    views
                                    ++ renderBorders borders
                                    ++ renderAutoComplete activeViewRect view buf global
                                )
                            , renderGlobal buf global
                            ]
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


pageTitle buf =
    if Buf.isDirty buf then
        "• " ++ buf.name

    else
        buf.name


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


renderGlobal buf global =
    let
        { fontInfo, isSafari, ime, lint, lineHeight } =
            global

        ime1 =
            { ime | isSafari = isSafari }
    in
    renderStatusBar
        fontInfo
        ime1
        (Buf.isDirty buf)
        buf.mode
        buf.continuation
        lint.items
        buf.name


renderStorage : Global -> Html Msg
renderStorage global =
    div [ style "display" "none" ]
        ([ lazy saveRegisters global.registers
         , lazy saveCwd global.cwd
         , div []
            (List.indexedMap
                (\i s ->
                    renderSessionStorageItem
                        ("exHistory[" ++ String.fromInt i ++ "]")
                        s
                )
                global.exHistory
            )
         ]
            ++ (case global.persistent of
                    Just { window, buffers } ->
                        [ lazy saveWindow window
                        , saveBuffers buffers
                        ]

                    _ ->
                        []
               )
        )


saveBuffers : List Buffer -> Html msg
saveBuffers buffers =
    Html.Keyed.node "div"
        []
        (List.indexedMap
            (\i buf ->
                ( String.fromInt buf.id, lazy2 saveBuffer i buf )
            )
            buffers
        )


saveBuffer : Int -> Buffer -> Html msg
saveBuffer i buf =
    buf
        |> bufferToString
        |> renderSessionStorageItem ("buffers[" ++ String.fromInt i ++ "]")


percentStr : Float -> String
percentStr f =
    String.fromFloat (f * 100) ++ "%"


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

        gutterWidth =
            totalLines |> String.fromInt |> String.length

        relativeGutterWidth =
            4

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
            Events.on "mousewheel"
                (Decode.map2
                    (toFloat
                        >> round
                        >> MouseWheel path1
                    )
                    (Decode.at [ "deltaY" ] Decode.int)
                    (Decode.at [ "deltaX" ] Decode.int)
                )
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
        [ renderGutter
            scrollingCss
            gutterWidth
            relativeZeroLine
            totalLines
            view.lines
        , lazy5 renderRelativeGutter
            lineHeight
            topOffsetPx
            height
            (relativeZeroLine - scrollTop1)
            (totalLines - scrollTop1)
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


renderLintStatus : List LintError -> Html msg
renderLintStatus items =
    let
        errors =
            items
                |> List.filter (.tipe >> (/=) "warning")
                |> List.length

        warnings =
            items
                |> List.filter (.tipe >> (==) "warning")
                |> List.length
    in
    div
        [ class "lint-status" ]
        [ span
            [ classList
                [ ( "highlight-errors-icon", errors > 0 ) ]
            ]
            [ i
                [ class "fas fa-times-circle" ]
                []
            , errors
                |> String.fromInt
                |> text
            ]
        , span
            [ classList [ ( "highlight-warnings-icon", warnings > 0 ) ] ]
            [ i
                [ class "fas fa-exclamation-triangle" ]
                []
            , warnings
                |> String.fromInt
                |> text
            ]
        ]


renderStatusBarRight : String -> String -> List LintError -> Html msg
renderStatusBarRight continuation name items =
    div [ class "status-right" ]
        [ div
            [ class "status-cmds" ]
            [ continuation
                |> parseKeys
                |> String.join ""
                |> text
            ]
        , div [ class "filename" ] [ text name ]
        , lazy renderLintStatus items
        ]


renderInputSafari : FontInfo -> Bool -> String -> Html Msg
renderInputSafari fontInfo isComposing _ =
    span
        ((if isComposing then
            []

          else
            [ style "opacity" "0" ]
         )
            ++ [ id "hidden-input"
               , contenteditable True
               , Events.custom "compositionstart"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionStart text
                                , stopPropagation = False
                                , preventDefault = False
                                }
                             --|> Debug.log "compositionstart"
                            )
                    )

               -- in safari keydown event is after `compositionstart` event
               , Events.custom "keydown"
                    (decodeKeyboardEvent False
                        |> Decode.map
                            (\key ->
                                { message = IMEMessage <| CompositionTry key
                                , stopPropagation = True
                                , preventDefault = True
                                }
                             --|> Debug.log "keydown"
                            )
                    )
               , Events.custom "compositionend"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionCommit text
                                , stopPropagation = True
                                , preventDefault = True
                                }
                             --|> Debug.log "compositionend"
                            )
                    )
               , Events.custom "textInput" <|
                    Decode.succeed
                        { message = NoneMessage
                        , stopPropagation = True
                        , preventDefault = True
                        }
               ]
        )
        []


renderInputChrome : FontInfo -> Bool -> String -> Html Msg
renderInputChrome fontInfo isComposing compositionText =
    span
        ((if isComposing then
            []

          else
            [ Events.custom "keydown"
                (decodeKeyboardEvent False
                    |> Decode.map
                        (\key ->
                            { message = IMEMessage (CompositionWait key)
                            , stopPropagation = True
                            , preventDefault = True
                            }
                         --|> Debug.log "keydown"
                        )
                )
            , style "opacity" "0"
            , property "textContent" (Encode.string "")
            ]
         )
            ++ [ id "hidden-input"
               , contenteditable True
               , Events.custom "compositionstart"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionStart text
                                , stopPropagation = False
                                , preventDefault = False
                                }
                            )
                     --|> Decode.map (Debug.log "compositionstart")
                    )
               , Events.custom "compositionupdate"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionStart text
                                , stopPropagation = False
                                , preventDefault = False
                                }
                            )
                     --|> Decode.map (Debug.log "compositionstart")
                    )
               , Events.custom "compositionend"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message =
                                    IMEMessage <|
                                        CompositionCommit <|
                                            --Debug.log "compositionend.data" <|
                                            text
                                , stopPropagation = True
                                , preventDefault = True
                                }
                            )
                    )
               ]
        )
        []


renderStatusBarLeft : FontInfo -> IME -> Mode -> Html Msg
renderStatusBarLeft fontInfo ime mode =
    let
        statusBar =
            Buf.getStatusBar mode

        divs =
            case statusBar.cursor of
                Just ( y, x ) ->
                    [ renderCursorInner True
                        True
                        fontInfo
                        ime
                        (B.fromString statusBar.text)
                        "ex-cursor"
                        0
                        x
                    ]

                _ ->
                    []
    in
    div [ class "status-left" ]
        (div
            [ class
                (if statusBar.error then
                    "status-error"

                 else
                    "status-info"
                )
            ]
            [ text
                (if statusBar.text == "-- Normal --" then
                    ""

                 else
                    statusBar.text
                )
            ]
            :: divs
        )


renderMatchedCursor :
    Bool
    -> FontInfo
    -> B.TextBuffer
    -> Mode
    -> Position
    -> Maybe ( Position, Position )
    -> List (Html Msg)
renderMatchedCursor isActive fontInfo lines mode cursor matchedCursor =
    case mode of
        Ex _ ->
            []

        _ ->
            case matchedCursor of
                Just ( a, b ) ->
                    [ a, b ]
                        |> List.filter ((/=) cursor)
                        |> List.map
                            (\( y, x ) ->
                                renderCursorInner isActive
                                    False
                                    fontInfo
                                    emptyIme
                                    lines
                                    "matched-cursor"
                                    y
                                    x
                            )

                _ ->
                    []


renderStatusBar :
    FontInfo
    -> IME
    -> Bool
    -> Mode
    -> String
    -> List LintError
    -> String
    -> Html Msg
renderStatusBar fontInfo ime dirty mode continuation items name =
    div
        [ class "status"
        , classList [ ( "dirty", dirty ) ]
        ]
        [ lazy3 renderStatusBarLeft fontInfo ime mode
        , lazy3 renderStatusBarRight continuation name items
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


renderVisual :
    FontInfo
    -> Int
    -> Int
    -> Mode
    -> B.TextBuffer
    -> Html msg
renderVisual fontInfo scrollTop height mode lines =
    case mode of
        Visual visual ->
            lazy4 renderSelections fontInfo scrollTop lines visual

        Ex { prefix, visual } ->
            let
                visual1 =
                    case visual of
                        Just v ->
                            case prefix of
                                ExSearch { match } ->
                                    case match of
                                        Just ( begin, end ) ->
                                            Just { v | end = begin }

                                        _ ->
                                            visual

                                _ ->
                                    visual

                        _ ->
                            Nothing
            in
            visual1
                |> Maybe.map (lazy4 renderSelections fontInfo scrollTop lines)
                |> Maybe.withDefault (text "")

        _ ->
            text ""


maybeToList : Maybe a -> List a
maybeToList mb =
    case mb of
        Just x ->
            [ x ]

        _ ->
            []


onKeyDownPressKeys replaceFullwithToHalfWidth =
    Events.custom "keydown"
        (decodeKeyboardEvent replaceFullwithToHalfWidth
            |> Decode.map
                (\key ->
                    { message = PressKeys key
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
        )


noCompositionInput : Html Msg
noCompositionInput =
    span
        [ id "hidden-input"
        , tabindex 0
        , autofocus True
        , onKeyDownPressKeys True
        , style "opacity" "0"
        ]
        []


renderCursorInner :
    Bool
    -> Bool
    -> FontInfo
    -> IME
    -> B.TextBuffer
    -> String
    -> Int
    -> Int
    -> Html Msg
renderCursorInner isActive isMainCursor fontInfo ime lines classname y x =
    let
        ( ( by, bx ), ( ey, ex ) ) =
            cursorPoint fontInfo lines y x

        imeIsActive =
            isMainCursor && ime.isActive

        imeIsComposing =
            imeIsActive && ime.isComposing
    in
    div
        ([ class "cursor"
         , class classname
         , classList [ ( "cursor-not-active", not isActive ) ]
         , style "left" <| px bx
         , style "top" <| rem y
         , style "width" <| px (ex - bx)
         ]
            ++ (if imeIsActive then
                    [ class "cursor-ime-active" ]

                else
                    []
               )
            ++ (if imeIsComposing then
                    [ class "cursor-ime-composing" ]

                else
                    []
               )
        )
        [ if imeIsActive then
            let
                renderInput =
                    if ime.isSafari then
                        renderInputSafari

                    else
                        renderInputChrome
            in
            lazy3 renderInput fontInfo imeIsComposing ime.compositionText

          else
            noCompositionInput
        ]


renderCursor :
    Bool
    -> FontInfo
    -> IME
    -> B.TextBuffer
    -> String
    -> Maybe Position
    -> Html Msg
renderCursor isActive fontInfo ime lines classname cursor =
    case cursor of
        Just ( y, x ) ->
            lazy8 renderCursorInner isActive True fontInfo ime lines classname y x

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


renderColumnGuide : FontInfo -> B.TextBuffer -> Maybe Position -> Html msg
renderColumnGuide fontInfo lines cursor =
    case cursor of
        Just ( y, x ) ->
            lazy4 renderCursorColumnInner fontInfo lines y x

        _ ->
            text ""


renderLineGuideInner : Int -> Html msg
renderLineGuideInner y =
    div
        [ class "guide line-guide"
        , style "top" <| rem y
        ]
        []


renderLineGuide : Maybe Position -> Html msg
renderLineGuide cursor =
    case cursor of
        Just ( y, x ) ->
            lazy renderLineGuideInner y

        _ ->
            text ""


renderHighlights :
    FontInfo
    -> Int
    -> B.TextBuffer
    -> Maybe ( VisualMode, List VisualMode )
    -> Html msg
renderHighlights fontInfo scrollTop lines highlights =
    case
        highlights
    of
        Just ( match, matches ) ->
            div
                [ class "highlights" ]
                (List.concatMap
                    (\{ tipe, begin, end } ->
                        renderRange fontInfo scrollTop tipe begin end lines False
                    )
                    (match :: matches)
                )

        _ ->
            text ""


renderSelections :
    FontInfo
    -> Int
    -> B.TextBuffer
    -> VisualMode
    -> Html msg
renderSelections fontInfo scrollTop lines { tipe, begin, end } =
    div
        [ class "selections" ]
        (renderRange fontInfo scrollTop tipe begin end lines False)


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


renderLint :
    String
    -> FontInfo
    -> Int
    -> B.TextBuffer
    -> List LintError
    -> Html msg
renderLint path fontInfo scrollTop lines items =
    div [ class "lints" ]
        (List.filterMap
            (\item ->
                if path == item.file then
                    let
                        ( ( by, _ ) as b, ( ey, _ ) as e ) =
                            Maybe.withDefault item.region item.subRegion

                        classname =
                            if item.tipe == "warning" then
                                "lint lint-warning"

                            else
                                "lint"
                    in
                    if not (ey < scrollTop || by >= scrollTop + 50) then
                        Just <|
                            div
                                [ class classname ]
                                (renderRange fontInfo
                                    scrollTop
                                    VisualChars
                                    b
                                    e
                                    lines
                                    True
                                )

                    else
                        Nothing

                else
                    Nothing
            )
            items
        )


{-| inclusive
-}
renderRange :
    FontInfo
    -> Int
    -> VisualType
    -> Position
    -> Position
    -> B.TextBuffer
    -> Bool
    -> List (Html msg)
renderRange fontInfo scrollTop tipe begin end lines excludeLineBreak_ =
    let
        regions =
            visualRegions False tipe begin end lines

        ( by, bx ) =
            Basics.min begin end

        ( ey, ex ) =
            Basics.max begin end

        excludeLineBreak =
            if tipe == VisualBlock then
                True

            else
                excludeLineBreak_
    in
    List.range by ey
        |> List.filter (\i -> i >= scrollTop && i < scrollTop + 50)
        |> List.filterMap
            (\row ->
                let
                    maxcol =
                        B.getLineMaxColumn row lines
                            - (if excludeLineBreak then
                                String.length B.lineBreak

                               else
                                0
                              )

                    maybeRegion =
                        case tipe of
                            VisualLine ->
                                Just ( 0, maxcol )

                            VisualBlock ->
                                let
                                    bx1 =
                                        Basics.min bx ex

                                    ex1 =
                                        Basics.max bx ex
                                in
                                if bx1 > maxcol then
                                    Nothing

                                else
                                    Just ( bx1, Basics.min maxcol ex1 )

                            _ ->
                                if by == ey then
                                    Just ( bx, ex )

                                else if row == by then
                                    Just ( bx, maxcol )

                                else if row == ey then
                                    Just ( 0, ex )

                                else
                                    Just ( 0, maxcol )
                in
                Maybe.map2
                    (\( bx_, ex_ ) line ->
                        let
                            styleTop =
                                row
                                    |> rem
                                    |> style "top"

                            styleLeft =
                                line
                                    |> stringWidth fontInfo 0 bx_
                                    |> px
                                    |> style "left"

                            styleWidth =
                                line
                                    |> stringWidth fontInfo bx_ ex_
                                    |> (+)
                                        (cursorCharWidth fontInfo
                                            ex_
                                            line
                                        )
                                    |> px
                                    |> style "width"
                        in
                        div
                            [ styleTop
                            , styleLeft
                            , styleWidth
                            ]
                            []
                    )
                    maybeRegion
                    (B.getLine row lines)
            )


renderGutterHighlight : Int -> Int -> Html ms
renderGutterHighlight offset highlightLine =
    div
        [ class "line-number-highlight"
        , style "top" <| rem offset
        ]
        [ text <| String.fromInt <| (highlightLine + 1) ]


renderGutterInner : Int -> Int -> List Int -> Html msg
renderGutterInner totalLines highlightLine viewLines =
    div
        [ class "gutter"
        , class "absolute-gutter"
        ]
        (List.map
            (\lineNumber ->
                if lineNumber < totalLines then
                    div
                        [ classList
                            [ ( "line-number-highlight"
                              , highlightLine == lineNumber
                              )
                            , ( "line-number", True )
                            ]
                        , style "top" <| rem lineNumber
                        ]
                        [ text (String.fromInt (lineNumber + 1)) ]

                else
                    div [] []
            )
            viewLines
        )


renderGutter :
    List (Attribute msg)
    -> Int
    -> Int
    -> Int
    -> List Int
    -> Html msg
renderGutter scrollingCss totalWidth highlightLine totalLines viewLines =
    div
        ([ class "gutter-container absolute-gutter-container"
         , style "width" <| ch <| totalWidth + 1
         ]
            ++ scrollingCss
        )
        [ lazy3 renderGutterInner totalLines highlightLine viewLines ]


renderAllRelativeNumbers : Int -> Int -> Html msg
renderAllRelativeNumbers low high =
    div
        [ class "gutter"
        , class "relative-gutter"
        ]
        ((List.range 1 low
            |> List.reverse
            |> List.map
                (\i ->
                    div [ class "line-number" ]
                        [ text <| String.fromInt i ]
                )
         )
            ++ (List.range 0 (high - 1)
                    |> List.map
                        (\i ->
                            div [ class "line-number" ]
                                [ text <| String.fromInt i ]
                        )
               )
        )


renderRelativeGutter : Int -> Int -> Int -> Int -> Int -> Html msg
renderRelativeGutter lineHeight topOffsetPx height zeroLine maxLine =
    div
        [ class "gutter-container"
        , class "relative-gutter-container"
        , style "top" <|
            String.fromInt ((zeroLine - height) * lineHeight - topOffsetPx)
                ++ "px"
        ]
        [ lazy2 renderAllRelativeNumbers
            height
            (Basics.min (maxLine - zeroLine) height)
        ]


renderTokens : List Token -> String -> Int -> List (Html msg)
renderTokens spans line i =
    case spans of
        sp :: rest ->
            let
                j =
                    i + sp.length

                -- FIXME: implement wrapping
                j1 =
                    Basics.min j 1000
            in
            span
                [ class sp.classname ]
                [ line
                    |> String.slice i j1
                    |> text
                ]
                :: (if j1 == j then
                        renderTokens rest line j

                    else
                        []
                   )

        _ ->
            -- this means we have broken syntax tokens
            if i < String.length line then
                [ span []
                    [ line
                        |> String.slice i (String.length line)
                        |> text
                    ]
                ]

            else
                []


scrollingStyle : Int -> Int -> Int -> Int -> Int -> List (Attribute msg)
scrollingStyle lineHeight topOffsetPx totalLines height scrollTop =
    [ style "top" <|
        String.fromInt -(topOffsetPx + scrollTop * lineHeight)
            ++ "px"
    , style "height" <| rem (totalLines + height)
    ]


renderLine : List Token -> String -> Html msg
renderLine tokens s =
    div []
        (renderTokens tokens s 0)


renderLines : B.TextBuffer -> Syntax -> List Int -> Html msg
renderLines lines syntax viewLines =
    div
        [ class "lines" ]
        (List.map
            (\lineNumber ->
                div
                    [ class "line"
                    , style "top" <| rem lineNumber
                    ]
                    (case B.getLine lineNumber lines of
                        Just text ->
                            case Array.get lineNumber syntax of
                                Just tokens ->
                                    [ lazy2 renderLine tokens text ]

                                _ ->
                                    [ lazy2 renderLine [] text ]

                        _ ->
                            []
                    )
            )
            viewLines
        )


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


saveWindow : Win.Window View -> Html msg
saveWindow win =
    win
        |> windowEncoder
        |> Encode.encode 0
        |> renderSessionStorageItem "window"


saveRegisters : Dict String RegisterText -> Html msg
saveRegisters registers =
    registers
        |> registerToString
        |> renderSessionStorageItem "registers"


saveCwd : String -> Html msg
saveCwd cwd =
    renderSessionStorageItem "cwd" cwd


renderSessionStorageItem : String -> String -> Html msg
renderSessionStorageItem key value =
    node "session-storage-item"
        [ attribute "key" key
        , attribute "value" value
        ]
        []
