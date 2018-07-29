module TestGenerated exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Update exposing (update, initMode)
import Model exposing (..)
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Message exposing (Msg(..))
import Parser as P exposing ((|.), (|=), Parser)
import Vim.Helper exposing (keyParser)
import Vim.AST exposing (ModeName(..), VisualType(..))
import Helper.Helper exposing (getLast, arrayInsert)
import Internal.Position exposing (Position)
import Regex as Re
import Elm.Array as Array
import Update.Buffer as Buf


log : String -> (a -> b) -> a -> a
log prefix f obj =
    let
        _ =
            Debug.log prefix (f obj)
    in
        obj


handleKeys : List Key -> Buffer -> Buffer
handleKeys keys buf =
    keys
        |> List.map PressKey
        |> List.foldl
            (\msg buf ->
                update msg buf
                    |> Tuple.first
            )
            buf


keysParser : Parser (List String)
keysParser =
    P.repeat P.zeroOrMore keyParser


emptyInsertMode : { autoComplete : Maybe a, startCursor : Position }
emptyInsertMode =
    { autoComplete = Nothing, startCursor = ( 0, 0 ) }


dataToBuffer : String -> Buffer
dataToBuffer s =
    emptyBuffer


formatBuffer : Buffer -> String
formatBuffer buf =
    let
        addPrefix prefix s =
            if String.isEmpty s then
                ""
            else
                prefix ++ s

        top =
            buf.lines
                |> B.sliceLines 0 buf.view.scrollTop
                |> B.mapLines (addPrefix "|       ")

        ( y, x ) =
            buf.cursor

        middle =
            buf.lines
                |> B.sliceLines buf.view.scrollTop
                    (buf.view.scrollTop + buf.view.size.height)
                |> B.mapLines (addPrefix "||      ")

        bottom =
            buf.lines
                |> B.sliceLines
                    (buf.view.scrollTop + buf.view.size.height)
                    (B.count buf.lines)
                |> B.mapLines (addPrefix "|       ")

        breakRegions buf =
            case buf.mode of
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
                                            B.getLineMaxColumn row buf.lines

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
                                        Maybe.map ((,) row) maybeRegion
                                )

                _ ->
                    case buf.cursor of
                        ( y, x ) ->
                            [ ( y, ( x, x ) ) ]

        insertRegions regions textLines =
            regions
                |> List.reverse
                |> List.foldl
                    (\( y, ( b, e ) ) lines ->
                        let
                            s =
                                if y == Tuple.first buf.cursor then
                                    let
                                        x =
                                            Tuple.second buf.cursor
                                    in
                                        ((String.repeat (8 + b) " ")
                                            ++ (String.repeat (x - b) "-")
                                            ++ "^"
                                            ++ (String.repeat (e - x) "-")
                                        )
                                else
                                    ((String.repeat (8 + b) " ")
                                        ++ (String.repeat (e - b + 1) "-")
                                    )
                        in
                            arrayInsert
                                (y + 1)
                                (s ++ "\n")
                                lines
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
            "~\n"
                |> List.repeat
                    (Basics.max
                        (buf.view.size.height
                            - (lines
                                |> List.filter isVisible
                                |> List.length
                              )
                        )
                        0
                    )
    in
        buf.mode
            |> Buf.getStatusBar
            |> .text
            |> List.singleton
            |> List.append (lines ++ emptyLines)
            |> String.join ""
            |> String.trim


type alias TestCase =
    { init : Buffer
    , tests :
        List
            { input : List String
            , result : String
            }
    }


newBuffer : Mode -> Position -> Int -> Int -> String -> Buffer
newBuffer mode cursor height scrollTop lines =
    let
        view =
            emptyBuffer.view
    in
        { emptyBuffer
            | cursor = cursor
            , cursorColumn = Tuple.second cursor
            , mode = mode
            , view =
                { view
                    | size =
                        { width = 100
                        , height = height
                        }
                    , scrollTop = scrollTop
                }
            , lines =
                B.fromString
                    (if String.endsWith B.lineBreak lines then
                        lines
                     else
                        lines ++ B.lineBreak
                    )
        }


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
                        |> Re.find (Re.AtMost 1)
                            (Re.regex "\\^")
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


parseScrollTop : List String -> Int
parseScrollTop textLines =
    textLines
        |> List.indexedMap
            (\i line ->
                if String.startsWith "||" line then
                    Just i
                else
                    Nothing
            )
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault 0


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


testDataParser : Parser TestCase
testDataParser =
    P.succeed
        (\buf tests ->
            { init = buf
            , tests = tests
            }
        )
        |. P.ignoreUntil "{"
        |= (P.ignoreUntil "\n}"
                |> P.source
                |> P.andThen
                    (\s ->
                        if s == "\n}" then
                            P.succeed emptyBuffer
                        else if String.startsWith "~\n" s then
                            let
                                height =
                                    s
                                        |> String.dropRight 2
                                        |> String.lines
                                        |> List.length

                                view =
                                    emptyBuffer.view
                            in
                                P.succeed
                                    { emptyBuffer
                                        | view =
                                            { emptyView
                                                | size =
                                                    { width = 1
                                                    , height = height
                                                    }
                                            }
                                    }
                        else
                            let
                                lines =
                                    s
                                        |> String.dropRight 2
                                        |> String.lines

                                textLines =
                                    lines
                                        |> List.filter isTextLine

                                height =
                                    lines
                                        |> List.filter isVisible
                                        |> List.length

                                cursor =
                                    parseCursor lines

                                scrollTop =
                                    parseScrollTop textLines

                                mode =
                                    lines
                                        |> getLast
                                        |> Maybe.withDefault ""
                                        |> parseMode cursor
                            in
                                textLines
                                    |> List.map (String.dropLeft 8)
                                    |> String.join "\n"
                                    |> (flip (++) "\n")
                                    |> newBuffer mode cursor height scrollTop
                                    |> P.succeed
                    )
           )
        |= P.repeat P.oneOrMore
            (P.succeed (\keys result -> { input = keys, result = result })
                |. P.ignoreUntil "\n>"
                |= (P.ignoreUntil "\n"
                        |> P.source
                        |> P.andThen
                            (\s ->
                                s
                                    |> String.trim
                                    |> P.run keysParser
                                    |> Result.withDefault []
                                    |> P.succeed
                            )
                   )
                |. P.ignoreUntil "\n{"
                |= (P.ignoreUntil "\n}"
                        |> P.source
                        |> P.map (String.slice 0 -1 >> String.trim)
                   )
            )


scanlInputs :
    List
        { a
            | input : List String
            , result : String
        }
    -> List { input : List String, result : String }
scanlInputs tests =
    tests
        |> List.foldl
            (\{ input, result } allTests ->
                case allTests of
                    x :: xs ->
                        { input = (x.input ++ input), result = result }
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
                                ++ toString (i + 1)
                                ++ "> "
                                ++ String.join "" input
                            )
                        <|
                            \_ ->
                                init
                                    |> handleKeys input
                                    |> formatBuffer
                                    |> Expect.equal result
                    )
                |> describe name

        Err _ ->
            test name <|
                \_ ->
                    Expect.fail "invalid test data"
