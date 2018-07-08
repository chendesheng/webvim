module View exposing (..)

import Html exposing (..)
import Html.Lazy exposing (..)
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
import Helper.Helper exposing (maybeAndThen2)
import Bitwise as BW


--import Regex exposing (regex)
--import Fuzzy exposing (FuzzyMatchItem)


pack : Int -> Int -> Int -> Int
pack offset a b =
    a
        |> BW.shiftLeftBy offset
        |> BW.or b


unpack : Int -> Int -> ( Int, Int )
unpack offset n =
    ( BW.shiftRightZfBy offset n
    , BW.and 0x0FFF n
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


vrView : Buffer -> Html msg
vrView buf =
    div [ class "vr-editor" ] [ view buf, view buf ]


view : Buffer -> Html msg
view buf =
    let
        { mode, cursor, lines, syntax, continuation, view, history } =
            buf

        scrollTop =
            view.scrollTop

        { width, height } =
            view.size

        totalLines =
            B.count lines - 1

        maybeCursor =
            case mode of
                Ex _ ->
                    Nothing

                _ ->
                    case cursor of
                        ( y, x ) ->
                            Just ( y - scrollTop, x )

        ( searchRange, scrollTop1 ) =
            case incrementSearch scrollTop height mode lines of
                Just ( r, t ) ->
                    ( Just r, t )

                Nothing ->
                    ( Nothing, scrollTop )

        relativeNumberLine =
            searchRange
                |> Maybe.map .begin
                |> Maybe.withDefault cursor
                |> Tuple.first

        matchedCursor =
            Maybe.map (Tuple.mapFirst (\y -> y - scrollTop))
                buf.view.matchedCursor

        matchedCursor2 =
            case buf.mode of
                Insert _ ->
                    maybeAndThen2
                        (\cursor matchedCursor ->
                            if cursor > matchedCursor then
                                case cursor of
                                    ( y, x ) ->
                                        Just ( y, Basics.max 0 (x - 1) )
                            else
                                Nothing
                        )
                        maybeCursor
                        matchedCursor

                _ ->
                    Nothing

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
    in
        div [ class "editor" ]
            ([ div [ class "buffer" ]
                [ renderGutter
                    (scrollTop1 + 1)
                    (Basics.min (scrollTop1 + height + 1) totalLines)
                    gutterWidth
                    scrollTop1
                    relativeNumberLine
                , lazy2 renderRelativeGutter
                    (relativeNumberLine - scrollTop1)
                    (Basics.min (scrollTop1 + height + 1) (totalLines - 1)
                        - relativeNumberLine
                    )
                , div [ class "lines-container" ]
                    (renderCursorColumn maybeCursor
                        :: renderLineGuide scrollTop1 maybeCursor
                        :: div [ class "ruler" ] []
                        :: renderVisual scrollTop1 height mode searchRange lines
                        ?:: (searchRange
                                |> Maybe.map
                                    (lazy3 renderHighlights scrollTop1 lines)
                            )
                        ?:: lazy3 renderLint scrollTop1 lines buf.lint.items
                        :: renderLines
                            scrollTop1
                            (height + 1)
                            lines
                            syntax
                        :: renderCursor "" maybeCursor
                        :: renderCursor "matched-cursor" matchedCursor
                        :: renderCursor "matched-cursor" matchedCursor2
                        :: renderTip scrollTop1
                            buf.lint.items
                            (Maybe.map
                                (\cur ->
                                    let
                                        ( y, x ) =
                                            cur
                                    in
                                        ( y + scrollTop1, x )
                                )
                                maybeCursor
                            )
                            showTip
                        ?:: []
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
    div [ class "lint-status" ]
        [ span []
            [ i [ class "fas fa-times-circle" ] []
            , items
                |> List.filter (.tipe >> ((/=) "warning"))
                |> List.length
                |> toString
                |> text
            ]
        , span []
            [ i [ class "fas fa-exclamation-triangle" ] []
            , items
                |> List.filter (.tipe >> ((==) "warning"))
                |> List.length
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
                        ""
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


renderStatusBar : Bool -> Mode -> String -> List LintError -> String -> Html msg
renderStatusBar dirty mode continuation items name =
    div
        [ class "status"
        , classList [ ( "dirty", dirty ) ]
        ]
        [ lazy renderStatusBarLeft mode
        , lazy3 renderStatusBarRight continuation name items
        ]


incrementSearch :
    Int
    -> Int
    -> Mode
    -> B.TextBuffer
    -> Maybe ( VisualMode, Int )
incrementSearch scrollTop height mode lines =
    (case mode of
        Ex { prefix, visual } ->
            case prefix of
                ExSearch { match } ->
                    case match of
                        Just ( begin, end ) ->
                            let
                                by =
                                    Basics.min
                                        (Tuple.first begin)
                                        (Tuple.first end)
                            in
                                Just
                                    ( { tipe = VisualChars
                                      , begin = begin
                                      , end = end
                                      }
                                    , Buf.bestScrollTop
                                        by
                                        height
                                        lines
                                        scrollTop
                                    )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing
    )


renderVisual :
    Int
    -> Int
    -> Mode
    -> Maybe VisualMode
    -> B.TextBuffer
    -> Maybe (Html msg)
renderVisual scrollTop height mode searchRange lines =
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


renderCursorColumn : Maybe Position -> Html msg
renderCursorColumn cursor =
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


renderLineGuide : Int -> Maybe Position -> Html msg
renderLineGuide scrollTop cursor =
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
renderTipInner packedCursor scrollTop items =
    let
        cursor =
            unpack 20 packedCursor

        distanceFrom ( y, x ) { region } =
            let
                ( y1, x1 ) =
                    Tuple.first region
            in
                ( abs (y1 - y), abs (x1 - x) )

        renderDetails top details =
            div
                [ style
                    [ ( "top", rem top ) ]
                , class "tip"
                ]
                [ text details ]
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
                        cursor >= begin && cursor <= end
                )
            |> List.sortBy (distanceFrom cursor)
            |> List.head
            |> Maybe.map
                (\item ->
                    renderDetails
                        (Tuple.first cursor - scrollTop + 1)
                        --(by - scrollTop)
                        (item.overview
                            ++ B.lineBreak
                            ++ item.details
                        )
                )
            |> Maybe.withDefault (text "")


renderTip :
    Int
    -> List LintError
    -> Maybe Position
    -> Bool
    -> Maybe (Html msg)
renderTip scrollTop items maybeCursor showTip =
    if showTip then
        maybeCursor
            |> Maybe.map
                (\( y, x ) ->
                    lazy3 renderTipInner (pack 20 y x) scrollTop items
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
        ( by, bx ) =
            Basics.min begin end

        ( ey, ex ) =
            Basics.max begin end
    in
        List.range by ey
            |> List.filter (\i -> i >= scrollTop && i < scrollTop + 50)
            |> List.map
                (\row ->
                    let
                        maxcol =
                            B.getLineMaxColumn row lines
                                - (if excludeLineBreak then
                                    String.length B.lineBreak
                                   else
                                    0
                                  )

                        ( b_, e_ ) =
                            case tipe of
                                VisualLine ->
                                    ( 0, maxcol )

                                VisualBlock ->
                                    ( bx, ex )

                                _ ->
                                    if by == ey then
                                        ( bx, ex )
                                    else if row == by then
                                        ( bx, maxcol )
                                    else if row == ey then
                                        ( 0, ex )
                                    else
                                        ( 0, maxcol )

                        e =
                            Basics.min maxcol e_

                        b =
                            Basics.min maxcol b_
                    in
                        (div
                            [ style
                                [ ( "left", ch b )
                                , ( "top", rem <| row - scrollTop )
                                , ( "width", ch <| e - b + 1 )
                                ]
                            ]
                            []
                        )
                )


renderGutterInner : Int -> Int -> Html msg
renderGutterInner begin end =
    div
        [ class "gutter"
        , class "absolute-gutter"
        ]
        (List.range begin end
            |> List.map
                (\i ->
                    div
                        [ class "line-number" ]
                        [ text <| toString i ]
                )
        )


renderGutterHighlight : Int -> Int -> Html ms
renderGutterHighlight scrollTop highlightLine =
    div
        [ class "line-number-highlight"
        , style [ ( "top", rem <| highlightLine - scrollTop ) ]
        ]
        [ text <| toString <| highlightLine + 1 ]


renderGutter : Int -> Int -> Int -> Int -> Int -> Html msg
renderGutter begin end totalWidth scrollTop highlightLine =
    div
        [ class "gutter-container"
        , style [ ( "width", ch <| totalWidth + 1 ) ]
        ]
        [ lazy2 renderGutterInner begin end
        , lazy2 renderGutterHighlight scrollTop highlightLine
        ]


renderRelativeGutter : Int -> Int -> Html msg
renderRelativeGutter topn bottomn =
    div [ class "gutter-container" ]
        [ div
            [ class "gutter"
            , class "relative-gutter"

            --, style [ ( "width", total |> toString |> String.length |> ch ) ]
            ]
            ((List.range 1 topn
                |> List.reverse
                |> List.map
                    (\i ->
                        div [ class "line-number" ]
                            [ text <| toString i ]
                    )
             )
                ++ [ div
                        [ class "line-number"
                        , class "current-line"
                        ]
                        [ text "0" ]
                   ]
                ++ (List.range 1 bottomn
                        |> List.map
                            (\i ->
                                div [ class "line-number" ]
                                    [ text <| toString i ]
                            )
                   )
            )
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


renderLinesInner : Int -> B.TextBuffer -> Syntax -> Html msg
renderLinesInner packed lines syntax =
    let
        ( scrollTop, height ) =
            unpack 12 packed
    in
        div
            [ class "lines" ]
            (lines
                |> B.indexedMapLinesToList scrollTop
                    (scrollTop + height)
                    (\n line ->
                        case Array.get n syntax of
                            Just tokens ->
                                div [ class "line" ]
                                    (renderTokens tokens line 0)

                            Nothing ->
                                div [ class "line" ] [ text line ]
                    )
            )


renderLines : Int -> Int -> B.TextBuffer -> Syntax -> Html msg
renderLines scrollTop height lines syntax =
    lazy3 renderLinesInner (pack 12 scrollTop height) lines syntax


renderAutoCompleteMenu : Bool -> Int -> Int -> AutoComplete -> Html msg
renderAutoCompleteMenu isEx viewScrollTop gutterWidth auto =
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
                            , ( "top", rem (y + 1 - viewScrollTop) )
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
