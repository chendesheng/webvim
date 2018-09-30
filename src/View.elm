module View exposing (..)

import Html exposing (..)
import Html.Lazy exposing (..)
import Html.Events as Events
import Model exposing (..)
import Internal.TextBuffer as B
import Array as Array exposing (Array)
import List
import Html.Attributes exposing (..)
import Internal.Position exposing (Position)
import Vim.AST exposing (VisualType(..))
import Vim.Helper exposing (parseKeys)
import Internal.Syntax exposing (Syntax, Token)
import String
import Update.Buffer as Buf
import Dict exposing (Dict)
import Bitwise as BW
import Update.Message exposing (Msg(..), IMEMsg(..))
import Update.Range exposing (visualRegions)
import Json.Decode as Decode
import Json.Encode as Encode
import Browser exposing (Document)
import Helper.KeyEvent exposing (decodeKeyboardEvent)


type alias Point =
    ( Int, Int )


cursorPoint : FontInfo -> B.TextBuffer -> Int -> Int -> ( Point, Point )
cursorPoint fontInfo lines y x =
    lines
        |> B.getLine y
        |> Maybe.map
            (\line ->
                let
                    x1 =
                        stringWidth fontInfo 0 x line

                    w =
                        cursorCharWidth fontInfo x line
                in
                    ( ( y, x1 ), ( y, x1 + w ) )
            )
        |> Maybe.withDefault ( ( 0, 0 ), ( 0, 0 ) )


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


page : Buffer -> Document Msg
page buf =
    { title = pageTitle buf
    , body = [ pageDom buf ]
    }


pageTitle buf =
    if Buf.isDirty buf then
        "• " ++ buf.name
    else
        buf.name


pageDom : Buffer -> Html Msg
pageDom buf =
    let
        { mode, cursor, lines, syntax, continuation, view, history, ime } =
            buf

        { fontInfo } =
            buf.config

        scrollTop =
            view.scrollTop

        height =
            view.size.height + 2

        topOffsetPx =
            remainderBy view.lineHeight view.scrollTopPx

        totalLines =
            B.count lines - 1

        maybeCursor =
            case mode of
                Ex _ ->
                    Nothing

                _ ->
                    Just cursor

        scrollTop1 =
            Buf.finalScrollTop buf

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
                    view.showTip

        gutterWidth =
            totalLines |> String.fromInt |> String.length

        relativeGutterWidth =
            4

        lineHeight =
            buf.view.lineHeight

        scrollingCss =
            scrollingStyle
                lineHeight
                topOffsetPx
                totalLines
                height
                scrollTop1

        mouseWheel h =
            Events.on "mousewheel"
                (Decode.map
                    (toFloat
                        >> (*) 1.8
                        >> floor
                        >> Basics.min (2 * h)
                        >> Basics.max (-2 * h)
                        >> MouseWheel
                    )
                    (Decode.at [ "deltaY" ] Decode.int)
                )
    in
        div
            [ class "editor" ]
            ([ div
                ([ class "buffer" ]
                    ++ case buf.mode of
                        Ex _ ->
                            []

                        Insert { autoComplete } ->
                            case autoComplete of
                                Just _ ->
                                    []

                                _ ->
                                    [ mouseWheel lineHeight ]

                        _ ->
                            [ mouseWheel lineHeight ]
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
                    (class "lines-container" :: scrollingCss)
                    (renderColumnGuide fontInfo lines maybeCursor
                        :: renderLineGuide maybeCursor
                        :: lazy5 renderVisual fontInfo scrollTop1 height mode lines
                        :: renderHighlights fontInfo scrollTop1 lines highlights
                        :: lazy4 renderLint fontInfo scrollTop1 lines buf.lint.items
                        :: lazy renderLines view.lines
                        :: div [ class "ruler" ] []
                        :: renderCursor fontInfo ime lines "" maybeCursor
                        :: renderTip
                            buf.view.size.width
                            buf.lint.items
                            maybeCursor
                            showTip
                        :: renderMatchedCursor
                            fontInfo
                            lines
                            mode
                            cursor
                            buf.view.matchedCursor
                    )
                ]
             , renderStatusBar
                fontInfo
                ime
                (Buf.isDirty buf)
                mode
                continuation
                buf.lint.items
                buf.name
             , div [ style "display" "none" ]
                ([ lazy saveBuffers buf.buffers
                 , lazy saveRegisters buf.registers
                 , lazy saveCwd buf.cwd
                 ]
                    ++ if buf.path == "" then
                        []
                       else
                        [ lazy3 saveActiveBuffer
                            buf.path
                            buf.history.version
                            cursor
                        ]
                )
             ]
                ++ (case buf.mode of
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
                                                (gutterWidth
                                                    + relativeGutterWidth
                                                )
                                            )
                                        |> maybeToList

                                _ ->
                                    []

                        Insert { autoComplete } ->
                            case autoComplete of
                                Just auto ->
                                    [ renderAutoCompleteMenu
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
                   )
            )


renderLintStatus : List LintError -> Html msg
renderLintStatus items =
    let
        errors =
            items
                |> List.filter (.tipe >> ((/=) "warning"))
                |> List.length

        warnings =
            items
                |> List.filter (.tipe >> ((==) "warning"))
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


renderInputSafari : FontInfo -> Bool -> Html Msg
renderInputSafari fontInfo isComposing =
    span
        ((if isComposing then
            [ Events.custom "keydown"
                (decodeKeyboardEvent True
                    |> Decode.map
                        (\key ->
                            if key == "<escape>" then
                                { message = PressKeys key
                                , stopPropagation = True
                                , preventDefault = True
                                }
                            else
                                { message = NoneMessage
                                , stopPropagation = False
                                , preventDefault = False
                                }
                         --|> Debug.log "keydown"
                        )
                )
            ]
          else
            [ onKeyDownPressKeys False
            , style "opacity" "0"
            ]
         )
            ++ [ id "hidden-input"
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
               , Events.custom "compositionend"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionCommit text
                                , stopPropagation = True
                                , preventDefault = True
                                }
                            )
                     --|> Decode.map (Debug.log "compositionend")
                    )
               , contenteditable True
               , Events.custom "textInput"
                    (Decode.succeed
                        { message = NoneMessage
                        , stopPropagation = True
                        , preventDefault = True
                        }
                    )
               ]
        )
        []


renderInputChrome : FontInfo -> IME -> Html Msg
renderInputChrome fontInfo ime =
    --let
    --_ =
    --Debug.log "renderInput.ime" ime
    --in
    span
        ((if ime.isComposing then
            [ property "textContent" (Encode.string ime.compositionText) ]
          else
            [ Events.custom "keydown"
                (decodeKeyboardEvent False
                    |> Decode.map
                        (\key ->
                            { message = IMEMessage (CompositionWait key)
                            , stopPropagation = True
                            , preventDefault = False
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
    FontInfo
    -> B.TextBuffer
    -> Mode
    -> Position
    -> Maybe ( Position, Position )
    -> List (Html Msg)
renderMatchedCursor fontInfo lines mode cursor matchedCursor =
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
                                renderCursorInner False
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
                (visual1
                    |> Maybe.map (lazy4 renderSelections fontInfo scrollTop lines)
                    |> Maybe.withDefault (text "")
                )

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
                 --|> Debug.log "keydown"
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
    -> FontInfo
    -> IME
    -> B.TextBuffer
    -> String
    -> Int
    -> Int
    -> Html Msg
renderCursorInner isMainCursor fontInfo ime lines classname y x =
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
                lazy2 renderInputSafari fontInfo imeIsComposing
              else
                noCompositionInput
            ]


renderCursor :
    FontInfo
    -> IME
    -> B.TextBuffer
    -> String
    -> Maybe Position
    -> Html Msg
renderCursor fontInfo ime lines classname cursor =
    case cursor of
        Just ( y, x ) ->
            lazy7 renderCursorInner True fontInfo ime lines classname y x

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
                        (renderRange fontInfo scrollTop tipe begin end lines False)
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
                , (case details of
                    RichText rt ->
                        class ""

                    PlainText s ->
                        style "max-width" "400px"
                  )
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
    FontInfo
    -> Int
    -> B.TextBuffer
    -> List LintError
    -> Html msg
renderLint fontInfo scrollTop lines items =
    let
        render classname ( begin, end ) scrollTop_ lines_ =
            div
                [ class classname ]
                (renderRange fontInfo
                    scrollTop_
                    VisualChars
                    begin
                    end
                    lines_
                    True
                )
    in
        div [ class "lints" ]
            (List.filterMap
                (\item ->
                    let
                        region =
                            Maybe.withDefault item.region item.subRegion

                        ( b, e ) =
                            region

                        ( by, _ ) =
                            b

                        ( ey, _ ) =
                            e

                        classname =
                            if item.tipe == "warning" then
                                "lint lint-warning"
                            else
                                "lint"
                    in
                        if not (ey < scrollTop || by >= scrollTop + 50) then
                            Just <| render classname region scrollTop lines
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


renderGutterInner : Int -> Int -> List (Maybe ViewLine) -> Html msg
renderGutterInner totalLines highlightLine viewLines =
    div
        [ class "gutter"
        , class "absolute-gutter"
        ]
        (List.filterMap
            (Maybe.andThen
                (\{ lineNumber } ->
                    if lineNumber < totalLines then
                        div
                            ([ classList
                                [ ( "line-number-highlight"
                                  , highlightLine == lineNumber
                                  )
                                , ( "line-number", True )
                                ]
                             , style "top" <| rem lineNumber
                             ]
                            )
                            [ text (String.fromInt (lineNumber + 1)) ]
                            |> Just
                    else
                        Nothing
                )
            )
            viewLines
        )


renderGutter :
    List (Attribute msg)
    -> Int
    -> Int
    -> Int
    -> List (Maybe ViewLine)
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
            (String.fromInt ((zeroLine - height) * lineHeight - topOffsetPx))
                ++ "px"
        ]
        [ renderAllRelativeNumbers
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
                (span
                    [ class sp.classname ]
                    [ line
                        |> String.slice i j1
                        |> text
                    ]
                )
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
        (String.fromInt -(topOffsetPx + scrollTop * lineHeight))
            ++ "px"
    , style "height" <| rem (totalLines + height)
    ]


renderLines : List (Maybe ViewLine) -> Html msg
renderLines viewLines =
    div
        [ class "lines" ]
        (List.filterMap
            (Maybe.map
                (\viewLine ->
                    div
                        [ class "line"
                        , style "top" <| rem viewLine.lineNumber
                        ]
                        (renderTokens viewLine.tokens viewLine.text 0)
                )
            )
            viewLines
        )


renderAutoCompleteMenu :
    Int
    -> Int
    -> Bool
    -> Int
    -> Int
    -> AutoComplete
    -> Html msg
renderAutoCompleteMenu lineHeight topOffsetPx isEx viewScrollTop gutterWidth auto =
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


saveActiveBuffer : String -> Int -> Position -> Html msg
saveActiveBuffer path version cursor =
    { path = path
    , version = version
    , cursor = cursor
    , content = Nothing
    , syntax = True
    }
        |> bufferInfoToString
        |> renderSessionStorageItem "activeBuffer"


saveBuffers : Dict String BufferInfo -> Html msg
saveBuffers buffers =
    buffers
        |> Dict.values
        |> buffersInfoToString
        |> renderSessionStorageItem "buffers"


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
