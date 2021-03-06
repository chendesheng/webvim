module TestGenerated exposing
    ( TestCase
    , TestInput(..)
    , applyPatchParser
    , deletePatchParser
    , emptyInsertMode
    , formatBuffer
    , genTest
    , handleKeys
    , ignoreSpaces
    , insertPatchParser
    , isTextLine
    , isVisible
    , log
    , newBuffer
    , parseCursor
    , parseMode
    , positionParser
    , pressKeysParser
    , scanlInputs
    , showPosition
    , showTestInput
    , testDataParser
    )

import Array as Array
import Expect exposing (Expectation)
import Helper.Helper
    exposing
        ( arrayInsert
        , chompUntilAfter
        , findIndex
        , getLast
        , isSpace
        , keepZeroOrMore
        , oneOrMore
        , rangeCount
        , regex
        , repeatParser
        , spaceInline
        )
import Internal.Position exposing (Position)
import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Window as Win
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Frame as Frame exposing (Frame)
import Model.Global exposing (..)
import Model.View exposing (emptyView)
import Parser as P exposing ((|.), (|=), Parser)
import Regex as Re
import Test exposing (..)
import Update exposing (update, updateActiveBuffer)
import Update.Buffer as Buf
import Update.Message exposing (Msg(..))
import Update.Vim exposing (initMode)
import Vim.AST exposing (ModeName(..), VisualType(..))
import Vim.Helper exposing (escapeKey, keyParser)


log : String -> (a -> b) -> a -> a
log prefix f obj =
    let
        _ =
            Debug.log prefix (f obj)
    in
    obj


type TestInput
    = InputKey String
    | ApplyPatch Patch


showPosition : Position -> String
showPosition ( y, x ) =
    "( " ++ String.fromInt y ++ ", " ++ String.fromInt x ++ " )"


showTestInput : TestInput -> String
showTestInput input =
    case input of
        InputKey key ->
            key

        ApplyPatch patch ->
            case patch of
                Insertion pos s ->
                    "+" ++ showPosition pos ++ " \"" ++ B.toString s ++ "\""

                Deletion b e ->
                    "-" ++ showPosition b ++ " " ++ showPosition e


handleKeys : List TestInput -> Global -> Global
handleKeys inputs global =
    List.foldl
        (\input global_ ->
            case input of
                InputKey key ->
                    global_
                        --|> Debug.log "buf_"
                        |> update (PressKeys <| escapeKey key)
                        |> Tuple.first

                --|> Debug.log "return buf"
                ApplyPatch patch ->
                    updateActiveBuffer (Buf.transaction [ patch ]) global_
        )
        global
        inputs


emptyInsertMode : { autoComplete : Maybe a, startCursor : Position }
emptyInsertMode =
    { autoComplete = Nothing, startCursor = ( 0, 0 ) }


formatBuffer : Global -> String
formatBuffer global =
    let
        buf =
            getActiveBuffer global
                --|> Debug.log "getActiveBuffer"
                |> Maybe.withDefault emptyBuffer

        view =
            global.window
                |> Win.getActiveFrame
                |> Maybe.andThen Frame.getActiveView
                |> Maybe.withDefault buf.view

        addPrefix prefix s =
            if String.isEmpty s then
                ""

            else
                prefix ++ s

        top =
            buf.lines
                |> B.sliceLines 0 view.scrollTop
                |> B.mapLines (addPrefix "|       ")

        ( y, x ) =
            view.cursor

        --_ =
        --    Debug.log "buf.view" buf.view
        --_ =
        --    Debug.log "buf.lines" buf.lines
        --_ =
        --    Debug.log "middle" middle
        middle =
            view.lines
                |> List.sort
                |> List.filterMap
                    (\n ->
                        B.getLine n buf.lines
                            |> Maybe.map (addPrefix "||      ")
                    )
                |> List.take view.size.height
                |> Array.fromList

        --middle1 =
        --    buf.lines
        --        |> B.sliceLines buf.view.scrollTop
        --            (buf.view.scrollTop + buf.view.size.height)
        --        |> B.mapLines (addPrefix "||      ")
        bottom =
            buf.lines
                |> B.sliceLines
                    (view.scrollTop + view.size.height)
                    (B.count buf.lines)
                |> B.mapLines (addPrefix "|       ")

        breakRegions buf_ =
            case buf_.mode of
                Visual { tipe, begin, end } ->
                    let
                        ( by, bx ) =
                            Basics.min begin end

                        ( ey, ex ) =
                            Basics.max begin end
                    in
                    List.range by ey
                        |> List.filterMap
                            (\row ->
                                let
                                    maxcol =
                                        B.getLineMaxColumn row buf_.lines

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
                                Maybe.map (Tuple.pair row) maybeRegion
                            )

                _ ->
                    case view.cursor of
                        ( y_, x_ ) ->
                            [ ( y_, ( x_, x_ ) ) ]

        insertRegions regions textLines =
            regions
                |> List.reverse
                |> List.foldl
                    (\( y_, ( b, e ) ) lines_ ->
                        let
                            s =
                                if y_ == Tuple.first buf.view.cursor then
                                    let
                                        x_ =
                                            Tuple.second buf.view.cursor
                                    in
                                    String.repeat (8 + b) " "
                                        ++ String.repeat (x_ - b) "-"
                                        ++ "^"
                                        ++ String.repeat (e - x_) "-"

                                else
                                    String.repeat (8 + b) " "
                                        ++ String.repeat (e - b + 1) "-"
                        in
                        arrayInsert
                            (y_ + 1)
                            (s ++ "\n")
                            lines_
                    )
                    textLines

        lines =
            Array.empty
                |> Array.append bottom
                |> Array.append middle
                |> Array.append top
                |> Array.filter (String.isEmpty >> not)
                |> insertRegions (breakRegions buf)
                |> Array.toList

        emptyLines =
            List.repeat
                (Basics.max
                    (buf.view.size.height
                        - (lines
                            |> List.filter isVisible
                            |> List.length
                          )
                    )
                    0
                )
                "~\n"
    in
    buf.mode
        |> Buf.getStatusBar
        |> .text
        |> List.singleton
        |> List.append (lines ++ emptyLines)
        |> String.join ""
        |> String.trim


type alias TestCase =
    { init : Global
    , tests :
        List
            { input : List TestInput
            , result : String
            }
    }


newBuffer : Mode -> Position -> Int -> Int -> String -> Global
newBuffer mode cursor height scrollTop text =
    let
        lines =
            B.fromString
                (if String.endsWith B.lineBreak text then
                    text

                 else
                    text ++ B.lineBreak
                )

        ( global, buf ) =
            createBuffer "" emptyGlobal

        view =
            { emptyView
                | cursor = cursor
                , cursorColumn = Tuple.second cursor
                , scrollTop = scrollTop
                , lines = rangeCount scrollTop (height + 2)
            }

        window =
            Frame.empty
                |> Frame.resize { width = 100, height = height }
                |> Frame.addOrActiveView view
                |> Win.initWindow
    in
    Buf.addBuffer
        { buf
            | mode = mode
            , lines = lines
            , view = view
        }
        { global | window = window }


isTextLine : String -> Bool
isTextLine =
    String.startsWith "|"


parseCursor : List String -> Position
parseCursor lines =
    lines
        |> List.indexedMap
            (\i line ->
                if String.startsWith " " line then
                    line
                        |> Re.findAtMost 1
                            (regex "\\^")
                        |> List.head
                        |> Maybe.map
                            (\m ->
                                ( (lines
                                    |> List.take i
                                    |> List.filter isTextLine
                                    |> List.length
                                  )
                                    - 1
                                , m.index - 8
                                )
                            )

                else
                    Nothing
            )
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault ( 0, 0 )


parseMode : Position -> String -> Mode
parseMode cursor statusBar =
    if
        String.startsWith
            "-- Insert --"
            statusBar
    then
        Insert
            { autoComplete = Nothing
            , startCursor = cursor
            , visual = Nothing
            }

    else if
        String.startsWith
            "-- Visual --"
            statusBar
    then
        Visual
            { tipe = VisualChars
            , begin = cursor
            , end = cursor
            }

    else if
        String.startsWith
            "-- Visual Line --"
            statusBar
    then
        Visual
            { tipe = VisualLine
            , begin = cursor
            , end = cursor
            }

    else if
        String.startsWith
            "-- Visual Block --"
            statusBar
    then
        Visual
            { tipe = VisualBlock
            , begin = cursor
            , end = cursor
            }

    else if
        String.startsWith
            "-- (insert) --"
            statusBar
    then
        TempNormal

    else if String.startsWith ":" statusBar then
        Ex
            { prefix = ExCommand
            , exbuf =
                emptyExBuffer
                    |> Buf.transaction
                        [ Insertion ( 0, 0 ) <|
                            B.fromString statusBar
                        ]
            , visual = Nothing
            , message = EmptyMessage
            }

    else if String.startsWith "/" statusBar then
        Ex
            { prefix =
                ExSearch
                    { forward = True
                    , match = Nothing
                    , highlights = []
                    }
            , exbuf =
                emptyExBuffer
                    |> Buf.transaction
                        [ Insertion ( 0, 0 ) <|
                            B.fromString statusBar
                        ]
            , visual = Nothing
            , message = EmptyMessage
            }

    else if String.startsWith "?" statusBar then
        Ex
            { prefix =
                ExSearch
                    { forward = False
                    , match = Nothing
                    , highlights = []
                    }
            , exbuf =
                emptyExBuffer
                    |> Buf.transaction
                        [ Insertion ( 0, 0 ) <|
                            B.fromString statusBar
                        ]
            , visual = Nothing
            , message = EmptyMessage
            }

    else if
        String.startsWith "-- Normal --"
            statusBar
    then
        Normal { message = EmptyMessage }

    else
        Normal
            { message =
                case
                    statusBar
                        |> String.split " "
                        |> List.head
                of
                    Just s ->
                        if String.isEmpty s then
                            EmptyMessage

                        else
                            InfoMessage s

                    _ ->
                        EmptyMessage
            }


isVisible : String -> Bool
isVisible line =
    String.startsWith "||" line
        || String.startsWith "~" line


ignoreSpaces : Parser ()
ignoreSpaces =
    P.chompWhile spaceInline


pressKeysParser : Parser (List TestInput)
pressKeysParser =
    P.succeed (List.map InputKey)
        |. chompUntilAfter "\n>"
        |. ignoreSpaces
        |= repeatParser keyParser


positionParser : Parser ( Int, Int )
positionParser =
    P.succeed Tuple.pair
        |. P.symbol "("
        |. ignoreSpaces
        |= P.int
        |. ignoreSpaces
        |. P.symbol ","
        |. ignoreSpaces
        |= P.int
        |. ignoreSpaces
        |. P.symbol ")"


insertPatchParser : Parser Patch
insertPatchParser =
    P.succeed Insertion
        |. P.symbol "+"
        |. ignoreSpaces
        |= positionParser
        |. ignoreSpaces
        |. P.symbol "\""
        |= (keepZeroOrMore ((/=) '"')
                |> P.map B.fromString
           )
        |. P.symbol "\""
        |. ignoreSpaces


deletePatchParser : Parser Patch
deletePatchParser =
    P.succeed Deletion
        |. P.symbol "-"
        |. ignoreSpaces
        |= positionParser
        |. ignoreSpaces
        |= positionParser
        |. ignoreSpaces


applyPatchParser : Parser Patch -> Parser TestInput
applyPatchParser p =
    P.map ApplyPatch p


testDataParser : Parser TestCase
testDataParser =
    P.succeed
        (\global tests ->
            { init = global
            , tests = tests
            }
        )
        |. chompUntilAfter "{"
        |= (P.chompUntil "\n}"
                |> P.getChompedString
                |> P.andThen
                    (\s ->
                        if s == "" then
                            P.succeed <|
                                newBuffer (Normal { message = EmptyMessage })
                                    ( 0, 0 )
                                    1
                                    0
                                    ""

                        else if String.startsWith "~\n" s then
                            let
                                height =
                                    s
                                        |> String.lines
                                        |> List.length
                            in
                            P.succeed <|
                                newBuffer (Normal { message = EmptyMessage })
                                    ( 0, 0 )
                                    height
                                    0
                                    ""

                        else
                            let
                                lines =
                                    s
                                        |> String.dropLeft 1
                                        |> String.lines

                                textLines =
                                    lines
                                        |> List.filter isTextLine

                                --|> Debug.log "lines"
                                height =
                                    lines
                                        |> List.filter isVisible
                                        |> List.length

                                cursor =
                                    parseCursor lines

                                scrollTop =
                                    textLines
                                        |> findIndex (String.startsWith "||")
                                        |> Maybe.withDefault 0

                                mode =
                                    lines
                                        |> getLast
                                        |> Maybe.withDefault ""
                                        |> parseMode cursor
                            in
                            textLines
                                |> List.map (String.dropLeft 8)
                                |> String.join "\n"
                                |> (\line -> line ++ "\n")
                                |> newBuffer mode cursor height scrollTop
                                |> P.succeed
                    )
           )
        |= repeatParser
            (P.succeed
                (\input result ->
                    { input = input
                    , result = result
                    }
                )
                |= P.oneOf
                    [ pressKeysParser
                    , P.succeed identity
                        |. chompUntilAfter "\n<"
                        |. ignoreSpaces
                        |= repeatParser
                            (P.oneOf
                                [ applyPatchParser insertPatchParser
                                , applyPatchParser deletePatchParser
                                ]
                            )
                    ]
                |. chompUntilAfter "\n{"
                |= (P.chompUntil "\n}"
                        |> P.getChompedString
                        |> P.map String.trim
                   )
                |. P.symbol "\n}"
            )


scanlInputs :
    List
        { a
            | input : List b
            , result : String
        }
    -> List { input : List b, result : String }
scanlInputs tests =
    tests
        |> List.foldl
            (\{ input, result } allTests ->
                case allTests of
                    x :: xs ->
                        { input = x.input ++ input, result = result }
                            :: x
                            :: xs

                    _ ->
                        [ { input = input, result = result } ]
            )
            []
        |> List.reverse


genTest : String -> String -> Test
genTest name data =
    case P.run testDataParser data of
        Ok { init, tests } ->
            tests
                |> scanlInputs
                |> List.indexedMap
                    (\i { input, result } ->
                        test
                            (name
                                ++ "."
                                ++ String.fromInt (i + 1)
                                ++ " ["
                                ++ (input
                                        |> List.map showTestInput
                                        |> String.join ", "
                                   )
                                ++ "]"
                            )
                        <|
                            \_ ->
                                init
                                    |> handleKeys input
                                    |> formatBuffer
                                    |> Expect.equal result
                    )
                |> describe name

        Err err ->
            test name <|
                \_ ->
                    Expect.fail <| "parse test data error: " ++ Debug.toString err
