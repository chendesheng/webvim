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
                |> arrayInsert (y - buf.view.scrollTop + 1)
                    (String.repeat (x + 8) " " ++ "^\n")

        bottom =
            buf.lines
                |> B.sliceLines
                    (buf.view.scrollTop + buf.view.size.height)
                    (B.count buf.lines)
                |> B.mapLines (addPrefix "|       ")

        lines =
            Array.empty
                |> Array.append bottom
                |> Array.append middle
                |> Array.append top
                |> Array.toList
                |> List.filter (String.isEmpty >> not)

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
                        |> P.map (String.slice 0 -1)
                   )
            )


genTest : String -> String -> Test
genTest name data =
    case P.run testDataParser data of
        Ok { init, tests } ->
            tests
                |> List.foldl
                    (\{ input, result } ( buf, allTests, i ) ->
                        let
                            buf1 =
                                handleKeys input buf
                        in
                            ( buf1
                            , (buf1
                                |> formatBuffer
                                |> Expect.equal (String.trim result)
                                |> always
                                |> test
                                    (toString i
                                        ++ "> "
                                        ++ String.join "" input
                                    )
                              )
                                :: allTests
                            , i + 1
                            )
                    )
                    ( init, [], 0 )
                |> (\( _, tests, _ ) -> List.reverse tests)
                |> describe name

        Err _ ->
            test name <|
                \_ ->
                    Expect.fail "invalid test data"
