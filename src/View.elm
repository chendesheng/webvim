module View exposing (..)

import Html exposing (..)
import Html.Lazy exposing (..)
import Html.Events as Events
import Model exposing (..)
import Internal.TextBuffer as B
import Elm.Array as Array
import List
import Html.Attributes exposing (..)
import Internal.Position exposing (Position)
import Vim.AST exposing (VisualType(..))
import Internal.Syntax exposing (Syntax, Token)
import String
import Elm.Array exposing (Array)
import Update.Buffer as Buf
import Dict exposing (Dict)
import Bitwise as BW
import Update.Message exposing (Msg(..))
import Update.Range exposing (visualRegions)
import Json.Decode as Decode


pack : Int -> Int -> Int -> Int
pack offset a b =
    a
        |> BW.shiftLeftBy offset
        |> BW.or b


unpack : Int -> Int -> ( Int, Int )
unpack offset n =
    ( BW.shiftRightZfBy offset n
    , BW.and (BW.shiftLeftBy offset 1 - 1) n
    )


rem : number -> String
rem n =
    toString n ++ "rem"


ch : number -> String
ch n =
    toString n ++ "ch"


translate : number -> number -> ( String, String )
translate x y =
    ( "transform"
    , "translate(" ++ ch x ++ ", " ++ rem y ++ ")"
    )


vrView : Buffer -> Html Msg
vrView buf =
    div [ class "vr-editor" ] [ view buf, view buf ]


view : Buffer -> Html Msg
view buf =
    let
        { mode, cursor, lines, syntax, continuation, view, history } =
            buf

        scrollTop =
            view.scrollTop

        height =
            view.size.height + 2

        topOffsetPx =
            Basics.rem view.scrollTopPx view.lineHeight

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

        searchRange =
            incrementSearchRegion mode

        relativeZeroLine =
            searchRange
                |> Maybe.map .begin
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
            totalLines |> toString |> String.length

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

        mouseWheel lineHeight =
            Events.on "mousewheel"
                (Decode.map
                    (toFloat
                        >> (*) 1.8
                        >> floor
                        >> Basics.min (2 * lineHeight)
                        >> Basics.max (-2 * lineHeight)
                        >> MouseWheel
                    )
                    (Decode.at [ "deltaY" ] Decode.int)
                )
    in
        div [ class "editor" ]
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
                , renderRelativeGutter
                    lineHeight
                    topOffsetPx
                    height
                    (relativeZeroLine - scrollTop1)
                    (totalLines - scrollTop1)
                , div
                    [ class "lines-container"
                    , style scrollingCss
                    ]
                    (renderColumnGuide maybeCursor
                        :: renderLineGuide maybeCursor
                        :: renderVisual scrollTop1 height mode lines
                        ?:: (searchRange
                                |> Maybe.map
                                    (lazy3 renderHighlights scrollTop1 lines)
                            )
                        ?:: lazy3 renderLint scrollTop1 lines buf.lint.items
                        :: lazy renderLines view.lines
                        :: div [ class "ruler" ] []
                        :: renderCursor "" maybeCursor
                        :: renderTip
                            buf.view.size.width
                            buf.lint.items
                            maybeCursor
                            showTip
                        ?:: renderMatchedCursor mode cursor buf.view.matchedCursor
                    )
                ]
             , renderStatusBar
                (Buf.isDirty buf)
                mode
                continuation
                buf.lint.items
                buf.name
             , div [ style [ ( "display", "none" ) ] ]
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
                    |> toString
                    |> text
                ]
            , span
                [ classList [ ( "highlight-warnings-icon", warnings > 0 ) ] ]
                [ i
                    [ class "fas fa-exclamation-triangle" ]
                    []
                , warnings
                    |> toString
                    |> text
                ]
            ]


renderStatusBarRight : String -> String -> List LintError -> Html msg
renderStatusBarRight continuation name items =
    div [ class "status-right" ]
        [ div [ class "status-cmds" ] [ text continuation ]
        , div [ class "filename" ] [ text name ]
        , lazy renderLintStatus items
        ]


renderStatusBarLeft : Mode -> Html msg
renderStatusBarLeft mode =
    let
        statusBar =
            Buf.getStatusBar mode
    in
        div [ class "status-left" ]
            [ div
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
            , renderCursor "" statusBar.cursor
            ]


renderMatchedCursor :
    Mode
    -> Position
    -> Maybe ( Position, Position )
    -> List (Html msg)
renderMatchedCursor mode cursor matchedCursor =
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
                                renderCursorInner "matched-cursor" y x
                            )

                _ ->
                    []


renderStatusBar : Bool -> Mode -> String -> List LintError -> String -> Html msg
renderStatusBar dirty mode continuation items name =
    div
        [ class "status"
        , classList [ ( "dirty", dirty ) ]
        ]
        [ lazy renderStatusBarLeft mode
        , lazy3 renderStatusBarRight continuation name items
        ]


incrementSearchRegion : Mode -> Maybe VisualMode
incrementSearchRegion mode =
    case mode of
        Ex { prefix, visual } ->
            case prefix of
                ExSearch { match } ->
                    case match of
                        Just ( begin, end ) ->
                            Just
                                { tipe = VisualChars
                                , begin = begin
                                , end = end
                                }

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


renderVisual :
    Int
    -> Int
    -> Mode
    -> B.TextBuffer
    -> Maybe (Html msg)
renderVisual scrollTop height mode lines =
    case mode of
        Visual visual ->
            Just <| lazy3 renderSelections scrollTop lines visual

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
                    |> Maybe.map (lazy3 renderSelections scrollTop lines)
                )

        _ ->
            Nothing


maybeToList : Maybe a -> List a
maybeToList mb =
    case mb of
        Just x ->
            [ x ]

        _ ->
            []


(?::) : Maybe a -> List a -> List a
(?::) item list =
    case item of
        Just x ->
            x :: list

        _ ->
            list
infixr 5 ?::


renderCursorInner : String -> Int -> Int -> Html msg
renderCursorInner classname y x =
    div
        [ class "cursor"
        , class classname
        , style
            [ ( "left", ch x )
            , ( "top", rem y )
            ]
        ]
        []


renderCursor : String -> Maybe Position -> Html msg
renderCursor classname cursor =
    case cursor of
        Just ( y, x ) ->
            lazy3 renderCursorInner classname y x

        _ ->
            text ""


renderCursorColumnInner : Int -> Html msg
renderCursorColumnInner x =
    div
        [ class "guide column-guide"
        , style
            [ ( "left", ch x )
            ]
        ]
        []


renderColumnGuide : Maybe Position -> Html msg
renderColumnGuide cursor =
    case cursor of
        Just ( y, x ) ->
            lazy renderCursorColumnInner x

        _ ->
            text ""


renderLineGuideInner : Int -> Html msg
renderLineGuideInner y =
    div
        [ class "guide line-guide"
        , style
            [ ( "top", rem y ) ]
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
    Int
    -> B.TextBuffer
    -> VisualMode
    -> Html msg
renderHighlights scrollTop lines { tipe, begin, end } =
    div
        [ class "highlights" ]
        (renderRange scrollTop tipe begin end lines False)


renderSelections :
    Int
    -> B.TextBuffer
    -> VisualMode
    -> Html msg
renderSelections scrollTop lines { tipe, begin, end } =
    div
        [ class "selections" ]
        (renderRange scrollTop tipe begin end lines False)


renderTipInner : Int -> Int -> List LintError -> Html msg
renderTipInner width packed items =
    let
        cursor =
            unpack 16 packed

        distanceFrom ( y, x ) { region } =
            let
                ( y1, x1 ) =
                    Tuple.first region
            in
                ( abs (y1 - y), abs (x1 - x) )

        renderDetails ( y, x ) details =
            div
                [ style
                    [ ( "top", rem <| y + 1 )

                    -- FIXME: hard code character width
                    , ( "left", ch (Basics.min x (width // 9 - 40)) )
                    ]
                , class "tip"
                ]
                [ div
                    [ class "tip-content" ]
                    [ text details ]
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
                        (if String.isEmpty item.overview then
                            item.details
                         else
                            item.overview ++ "\n" ++ item.details
                        )
                )
            |> Maybe.withDefault (text "")


renderTip :
    Int
    -> List LintError
    -> Maybe Position
    -> Bool
    -> Maybe (Html msg)
renderTip width items maybeCursor showTip =
    if showTip then
        maybeCursor
            |> Maybe.map
                (\( y, x ) ->
                    lazy3 renderTipInner width (pack 16 y x) items
                )
    else
        Nothing


renderLint :
    Int
    -> B.TextBuffer
    -> List LintError
    -> Html msg
renderLint scrollTop lines items =
    let
        render classname ( begin, end ) scrollTop lines =
            div
                [ class classname ]
                (renderRange scrollTop VisualChars begin end lines True)
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
    Int
    -> VisualType
    -> Position
    -> Position
    -> B.TextBuffer
    -> Bool
    -> List (Html msg)
renderRange scrollTop tipe begin end lines excludeLineBreak =
    let
        regions =
            visualRegions False tipe begin end lines

        ( by, bx ) =
            Basics.min begin end

        ( ey, ex ) =
            Basics.max begin end
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
                        Maybe.map
                            (\( bx, ex ) ->
                                (div
                                    [ style
                                        [ ( "left", ch bx )
                                        , ( "top", rem row )
                                        , ( "width", ch <| ex - bx + 1 )
                                        ]
                                    ]
                                    []
                                )
                            )
                            maybeRegion
                )


renderGutterHighlight : Int -> Int -> Html ms
renderGutterHighlight offset highlightLine =
    div
        [ class "line-number-highlight"
        , style [ ( "top", rem offset ) ]
        ]
        [ text <| toString <| (highlightLine + 1) ]


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
                             , style [ ( "top", rem lineNumber ) ]
                             ]
                            )
                            [ text (toString (lineNumber + 1)) ]
                            |> Just
                    else
                        Nothing
                )
            )
            viewLines
        )


renderGutter :
    List ( String, String )
    -> Int
    -> Int
    -> Int
    -> List (Maybe ViewLine)
    -> Html msg
renderGutter scrollingCss totalWidth highlightLine totalLines viewLines =
    div
        [ class "gutter-container absolute-gutter-container"
        , style <|
            ( "width", ch <| totalWidth + 1 )
                :: scrollingCss
        ]
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
                        [ text <| toString i ]
                )
         )
            ++ (List.range 0 (high - 1)
                    |> List.map
                        (\i ->
                            div [ class "line-number" ]
                                [ text <| toString i ]
                        )
               )
        )


renderRelativeGutter : Int -> Int -> Int -> Int -> Int -> Html msg
renderRelativeGutter lineHeight topOffsetPx height zeroLine maxLine =
    div
        [ class "gutter-container"
        , class "relative-gutter-container"
        , style
            [ ( "top"
              , toString ((zeroLine - height) * lineHeight - topOffsetPx)
                    ++ "px"
              )
            ]
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
            in
                (span
                    [ class sp.classname ]
                    [ line
                        |> String.slice i j
                        |> text
                    ]
                )
                    :: (renderTokens rest line j)

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


scrollingStyle : Int -> Int -> Int -> Int -> Int -> List ( String, String )
scrollingStyle lineHeight topOffsetPx totalLines height scrollTop =
    [ ( "top"
      , toString -(topOffsetPx + scrollTop * lineHeight) ++ "px"
      )
    , ( "height", rem (totalLines + height) )
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
                        , style [ ( "top", rem viewLine.lineNumber ) ]
                        ]
                        (renderTokens viewLine.syntax viewLine.text 0)
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
        { matches, select, scrollTop, pos } =
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
                render i j matched =
                    renderSpan (String.slice i j s) matched
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
                        [ class "auto-complete-ex" ]
                    else
                        [ style
                            [ ( "left", ch (x + gutterWidth) )
                            , ( "top"
                              , toString
                                    ((y + 1 - viewScrollTop)
                                        * lineHeight
                                        - topOffsetPx
                                    )
                                    ++ "px"
                              )
                            ]
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
