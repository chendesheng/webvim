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


--import Regex exposing (regex)
--import Fuzzy exposing (FuzzyMatchItem)


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


view : Model -> Html msg
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
                |> Maybe.withDefault buf.cursor
                |> Tuple.first

        matchedCursor =
            buf.view.matchedCursor
                |> Maybe.map (Tuple.mapFirst (\y -> y - scrollTop))

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

        gutterWidth =
            totalLines |> toString |> String.length

        relativeGutterWidth =
            4
    in
        div [ class "editor" ]
            ([ div [ class "buffer" ]
                [ lazy3 renderGutter
                    (scrollTop1 + 1)
                    (Basics.min (scrollTop1 + height + 1) totalLines)
                    gutterWidth
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
                        :: lazy2 renderCursor "matched-cursor" matchedCursor
                        :: lazy2 renderCursor "matched-cursor" matchedCursor2
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
                            view.showTip
                        ?:: []
                    )
                ]
             , renderStatusBar
                (Buf.isDirty buf)
                mode
                continuation
                buf.lint.count
                buf.name
             , div [ style [ ( "display", "none" ) ] ]
                ([ lazy saveBuffers buf.buffers
                 , lazy saveRegisters buf.registers
                 ]
                    ++ if buf.path == "" then
                        []
                       else
                        [ lazy3 saveActiveBuffer
                            buf.path
                            cursor
                            scrollTop
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
                                                (gutterWidth + relativeGutterWidth)
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


renderStatusBar : Bool -> Mode -> String -> Int -> String -> Html msg
renderStatusBar dirty mode continuation errorsCnt name =
    let
        statusBar =
            getStatusBar mode
    in
        div
            [ class "status"
            , classList [ ( "dirty", dirty ) ]
            ]
            [ div [] [ text statusBar.text ]
            , renderCursor "" statusBar.cursor
            , div [ class "status-right" ]
                [ div [ class "status-cmds" ] [ text continuation ]
                , div [ class "filename" ] [ text name ]
                , div [ class "lint-status" ]
                    [ i [ class "far fa-times-circle" ] []
                    , text <| toString errorsCnt
                    ]
                ]
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
                                    , Buf.bestScrollTop by height lines scrollTop
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


renderCursor : String -> Maybe Position -> Html msg
renderCursor classname cursor =
    case cursor of
        Just ( y, x ) ->
            div
                [ class "cursor"
                , class classname
                , style
                    [ ( "left", ch x )
                    , ( "top", rem y )
                    ]
                ]
                []

        _ ->
            text ""


renderCursorColumn : Maybe Position -> Html msg
renderCursorColumn cursor =
    case cursor of
        Just ( y, x ) ->
            div
                [ class "guide column-guide"
                , style
                    [ ( "left", ch x )
                    ]
                ]
                []

        _ ->
            text ""


renderLineGuide : Int -> Maybe Position -> Html msg
renderLineGuide scrollTop cursor =
    case cursor of
        Just ( y, x ) ->
            div
                [ class "guide line-guide"
                , style
                    [ ( "top", rem y ) ]
                ]
                []

        _ ->
            text ""


getStatusBar : Mode -> { text : String, cursor : Maybe Position }
getStatusBar mode =
    case mode of
        Normal ->
            { text = "-- Normal --"
            , cursor = Nothing
            }

        Visual { tipe } ->
            { text =
                case tipe of
                    VisualLine ->
                        "-- Visual Line --"

                    VisualBlock ->
                        "-- Visual Block --"

                    _ ->
                        "-- Visual --"
            , cursor = Nothing
            }

        Insert _ ->
            { text = "-- Insert --"
            , cursor = Nothing
            }

        TempNormal ->
            { text = "-- (Insert) --"
            , cursor = Nothing
            }

        Ex { exbuf } ->
            { text = B.toString exbuf.lines
            , cursor = Just exbuf.cursor
            }


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


renderTip :
    Int
    -> List LintError
    -> Maybe Position
    -> Bool
    -> Maybe (Html msg)
renderTip scrollTop items maybeCursor showTip =
    if showTip then
        maybeCursor
            |> Maybe.andThen
                (\cursor ->
                    let
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

                        ( by, bx ) =
                            b

                        e1 =
                            e
                                |> Tuple.mapSecond (\x -> Basics.max 0 (x - 1))
                                |> Basics.max b

                        ( ey, _ ) =
                            e1
                    in
                        if not (ey < scrollTop || by >= scrollTop + 50) then
                            Just <| render "lint" ( b, e1 ) scrollTop lines
                        else
                            Nothing
                )
                items
            )


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

                        ( a, b ) =
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

                        width =
                            (b - a + 1)
                                - (if excludeLineBreak && b == maxcol then
                                    1
                                   else
                                    0
                                  )
                    in
                        (div
                            [ style
                                [ ( "left", ch a )
                                , ( "top", rem (row - scrollTop) )
                                , ( "width", ch width )
                                ]
                            ]
                            []
                        )
                )


renderGutter : Int -> Int -> Int -> Html msg
renderGutter begin end totalWidth =
    div [ class "gutter-container" ]
        [ div
            [ class "gutter"
            , class "absolute-gutter"
            , style [ ( "width", ch totalWidth ) ]
            ]
            (List.range begin end
                |> List.map
                    (\i ->
                        div
                            [ class "line-number" ]
                            [ text <| toString i ]
                    )
            )
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
                        []
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
            []


renderLines : Int -> Int -> B.TextBuffer -> Syntax -> Html msg
renderLines scrollTop height lines syntax =
    div
        [ class "lines"

        --, style [ translate 0 -scrollTop ]
        ]
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


saveActiveBuffer : String -> Position -> Int -> Html msg
saveActiveBuffer path cursor scrollTop =
    { path = path
    , cursor = cursor
    , scrollTop = scrollTop
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


renderSessionStorageItem : String -> String -> Html msg
renderSessionStorageItem key value =
    node "session-storage-item"
        [ attribute "key" key
        , attribute "value" value
        ]
        []
