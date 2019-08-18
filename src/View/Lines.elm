module View.Lines exposing
    ( renderHighlights
    , renderLines
    , renderLint
    , renderVisual
    )

import Array
import Font exposing (FontInfo, cursorCharWidth, stringWidth)
import Helper.Helper exposing (px, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Internal.Syntax exposing (Syntax, Token, TokenType(..))
import Internal.TextBuffer as B
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import TreeSitter as TS
import Update.Range exposing (visualRegions)
import Vim.AST exposing (VisualType(..))


renderTokens : List Token -> String -> Int -> String -> List (Html msg)
renderTokens spans line i lastClassName =
    case spans of
        sp :: rest ->
            if sp.length == 0 then
                if i < 1000 then
                    renderTokens rest line i sp.classname

                else
                    []

            else
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
                            renderTokens rest line j sp.classname

                        else
                            []
                       )

        _ ->
            -- fill rest with last classname
            if i < String.length line then
                [ span
                    (if String.isEmpty lastClassName then
                        []

                     else
                        [ class lastClassName ]
                    )
                    [ line
                        |> String.slice i (String.length line)
                        |> text
                    ]
                ]

            else
                []


renderLine : List Token -> String -> Html msg
renderLine tokens s =
    div []
        (renderTokens tokens s 0 "")


renderLines : Int -> B.TextBuffer -> Syntax -> List Int -> Html msg
renderLines scrollTop lines syntax viewLines =
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
                            case Array.get (lineNumber - scrollTop) syntax of
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


renderLines2 : B.TextBuffer -> Syntax -> List Int -> Html msg
renderLines2 lines syntax viewLines =
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



-- Decorations


renderHighlights :
    FontInfo
    -> Int
    -> B.TextBuffer
    -> Maybe ( VisualMode, List VisualMode )
    -> Html msg
renderHighlights fontInfo scrollTop lines highlights =
    case highlights of
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
                                        Just ( begin, _ ) ->
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
