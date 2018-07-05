module TestUpdate exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Update exposing (update)
import Model exposing (..)
import Update.Buffer as Buf
import Internal.TextBuffer as B exposing (Patch(..))
import Update.Message exposing (Msg(..))
import Parser as P exposing ((|.), (|=), Parser)
import Dict
import Maybe
import Vim.Helper exposing (keyParser)
import Vim.AST exposing (VisualType(..))
import Elm.Array as Array
import Internal.Jumps exposing (Location)
import Internal.Position exposing (Position)


handleKeys : List Key -> Buffer -> Buffer
handleKeys keys model =
    keys
        |> List.map PressKey
        |> List.foldl
            (\msg model ->
                update msg model
                    |> Tuple.first
            )
            model


keysParser : Parser (List String)
keysParser =
    P.repeat P.zeroOrMore keyParser


emptyInsertMode : { autoComplete : Maybe a, startCursor : Position }
emptyInsertMode =
    { autoComplete = Nothing, startCursor = ( 0, 0 ) }


exModeCasesBuf : Buffer
exModeCasesBuf =
    { emptyBuffer | lines = B.fromString "abc\ndef\ndef\n" }


exModeCases : List ( String, Buffer )
exModeCases =
    [ ( "v/ef<cr>"
      , { exModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualChars
                    , begin = ( 0, 0 )
                    , end = ( 1, 1 )
                    }
            , cursor = ( 1, 1 )
            , cursorColumn = 1
            , continuation = "v"
            , last =
                { emptyLast
                    | matchString = Just ( "ef", True )
                    , ex = "ef"
                }
        }
      )
    , ( "v/ef<esc>"
      , { exModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualChars
                    , begin = ( 0, 0 )
                    , end = ( 0, 0 )
                    }
            , last =
                { emptyLast | ex = "ef" }
            , continuation = "v"
        }
      )
    ]


emptyLast :
    { matchChar : Maybe a
    , matchString : Maybe a1
    , inserts : String
    , visual : String
    , ex : String
    , indent : Int
    , jumpToTag : Maybe Location
    , motionFailed : Bool
    }
emptyLast =
    { matchChar = Nothing
    , matchString = Nothing
    , inserts = ""
    , visual = ""
    , ex = ""
    , indent = 0
    , jumpToTag = Nothing
    , motionFailed = False
    }


visualModeCasesBuf : Buffer
visualModeCasesBuf =
    { emptyBuffer | lines = B.fromString "123\n456\n" }


visualModeCases : List ( String, Buffer )
visualModeCases =
    [ ( "v"
      , { visualModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualChars
                    , begin = ( 0, 0 )
                    , end = ( 0, 0 )
                    }
            , continuation = "v"
        }
      )
    , ( "V"
      , { visualModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualLine
                    , begin = ( 0, 0 )
                    , end = ( 0, 0 )
                    }
            , continuation = "V"
        }
      )
    , ( "<c-v>"
      , { visualModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualBlock
                    , begin = ( 0, 0 )
                    , end = ( 0, 0 )
                    }
            , continuation = "<c-v>"
        }
      )
    , ( "vv"
      , visualModeCasesBuf
      )
    , ( "vw"
      , { visualModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualChars
                    , begin = ( 0, 0 )
                    , end = ( 1, 0 )
                    }
            , cursor = ( 1, 0 )
            , last = { emptyLast | visual = "w" }
            , continuation = "v"
        }
      )
    , ( "vwl"
      , { visualModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualChars
                    , begin = ( 0, 0 )
                    , end = ( 1, 1 )
                    }
            , cursor = ( 1, 1 )
            , cursorColumn = 1
            , last = { emptyLast | visual = "wl" }
            , continuation = "v"
        }
      )
    , ( "lvh"
      , { visualModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualChars
                    , begin = ( 0, 1 )
                    , end = ( 0, 0 )
                    }
            , last = { emptyLast | visual = "h" }
            , continuation = "v"
        }
      )
    , ( "vo"
      , { visualModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualChars
                    , begin = ( 0, 0 )
                    , end = ( 0, 0 )
                    }
            , last = { emptyLast | visual = "o" }
            , continuation = "v"
        }
      )
    , ( "vlo"
      , { visualModeCasesBuf
            | mode =
                Visual
                    { tipe = VisualChars
                    , begin = ( 0, 1 )
                    , end = ( 0, 0 )
                    }
            , last = { emptyLast | visual = "lo" }
            , continuation = "v"
        }
      )
    , ( "vc"
      , { visualModeCasesBuf
            | mode = Insert emptyInsertMode
            , lines = B.fromString "23\n456\n"
            , registers = Dict.fromList [ ( "\"", Text "1" ) ]
            , continuation = "vc"
        }
      )
    , ( "vlc"
      , { visualModeCasesBuf
            | mode = Insert emptyInsertMode
            , lines = B.fromString "3\n456\n"
            , last = { emptyLast | visual = "l" }
            , registers = Dict.fromList [ ( "\"", Text "12" ) ]
            , continuation = "vc"
        }
      )
    , ( "vlcx<esc>"
      , { visualModeCasesBuf
            | lines = B.fromString "x3\n456\n"
            , last = { emptyLast | inserts = "x", visual = "l" }
            , dotRegister = "v<visual>c<inserts><esc>"
            , registers = Dict.fromList [ ( "\"", Text "12" ) ]
        }
      )
    , ( "vc11<esc>vc22<esc>"
      , { visualModeCasesBuf
            | mode = Normal { message = EmptyMessage }
            , cursor = ( 0, 2 )
            , cursorColumn = 2
            , lines = B.fromString "12223\n456\n"
            , last = { emptyLast | inserts = "22" }
            , dotRegister = "v<visual>c<inserts><esc>"
            , registers = Dict.fromList [ ( "\"", Text "1" ) ]
        }
      )
    ]


joinCasesBuf : Buffer
joinCasesBuf =
    { emptyBuffer
        | lines = B.fromString """123
456
  789


"""
    }


joinCases : List ( String, Buffer )
joinCases =
    [ ( "J"
      , { joinCasesBuf
            | lines = B.fromString "123 456\n  789\n\n\n"
            , cursor = ( 0, 3 )
            , cursorColumn = 3
        }
      )
    , ( "jgJ"
      , { joinCasesBuf
            | lines = B.fromString "123\n456  789\n\n\n"
            , cursor = ( 1, 3 )
            , cursorColumn = 3
        }
      )
    , ( "jjJ"
      , { joinCasesBuf
            | lines = B.fromString "123\n456\n  789 \n\n"
            , cursor = ( 2, 5 )
            , cursorColumn = 5
        }
      )
    , ( "jjgJ"
      , { joinCasesBuf
            | lines = B.fromString "123\n456\n  789\n\n"
            , cursor = ( 2, 4 )
            , cursorColumn = 4
        }
      )
    , ( "jjjJ"
      , { joinCasesBuf
            | lines = B.fromString "123\n456\n  789\n\n"
            , cursor = ( 3, 0 )
            , cursorColumn = 0
        }
      )

    -- FIXME: , ( "jjjjjJ", { joinCasesBuf | cursor = ( 4, 0 ) } )
    , ( "vjjjjjJ"
      , { joinCasesBuf
            | cursor = ( 0, 11 )
            , cursorColumn = 11
            , lines = B.fromString "123 456 789 \n"
        }
      )
    , ( "A<space><esc>J"
      , { joinCasesBuf
            | lines = B.fromString "123 456\n  789\n\n\n"
            , cursor = ( 0, 4 )
            , cursorColumn = 4
        }
      )
    , ( "A<space><esc>gJ"
      , { joinCasesBuf
            | lines = B.fromString "123 456\n  789\n\n\n"
            , cursor = ( 0, 4 )
            , cursorColumn = 4
        }
      )
    ]


jumpsCasesBuf : Buffer
jumpsCasesBuf =
    let
        view =
            emptyBuffer.view

        jumps =
            emptyBuffer.jumps
    in
        { emptyBuffer
            | lines = B.fromString """123
456
789
abc
def
"""
            , view = { view | size = { width = 20, height = 6 } }
            , path = "testpath"
            , jumps = jumps
        }


jumpsCases : List ( String, Buffer )
jumpsCases =
    [ ( "G"
      , { jumpsCasesBuf
            | jumps =
                { backwards = [ { path = "testpath", cursor = ( 0, 0 ) } ]
                , forwards = []
                }
        }
      )
    , ( "GM"
      , { jumpsCasesBuf
            | jumps =
                { backwards =
                    [ { path = "testpath", cursor = ( 4, 0 ) }
                    , { path = "testpath", cursor = ( 0, 0 ) }
                    ]
                , forwards = []
                }
        }
      )
    , ( "GMH"
      , { jumpsCasesBuf
            | jumps =
                { backwards =
                    [ { path = "testpath", cursor = ( 2, 0 ) }
                    , { path = "testpath", cursor = ( 4, 0 ) }
                    , { path = "testpath", cursor = ( 0, 0 ) }
                    ]
                , forwards = []
                }
        }
      )
    , ( "GMH<c-o>"
      , { jumpsCasesBuf
            | jumps =
                { backwards =
                    [ { path = "testpath", cursor = ( 4, 0 ) }
                    ]
                , forwards =
                    [ { path = "testpath", cursor = ( 2, 0 ) }
                    , { path = "testpath", cursor = ( 0, 0 ) }
                    ]
                }
        }
      )
    , ( "GMH<c-o><c-o>"
      , { jumpsCasesBuf
            | jumps =
                { backwards =
                    []
                , forwards =
                    [ { path = "testpath", cursor = ( 4, 0 ) }
                    , { path = "testpath", cursor = ( 2, 0 ) }
                    , { path = "testpath", cursor = ( 0, 0 ) }
                    ]
                }
        }
      )
    , ( "GMH<c-o><c-o><tab>"
      , { jumpsCasesBuf
            | jumps =
                { backwards =
                    [ { path = "testpath", cursor = ( 4, 0 ) }
                    ]
                , forwards =
                    [ { path = "testpath", cursor = ( 2, 0 ) }
                    , { path = "testpath", cursor = ( 0, 0 ) }
                    ]
                }
        }
      )
    , ( "GMH<c-o><c-o><tab>L"
      , { jumpsCasesBuf
            | jumps =
                { backwards =
                    [ { path = "testpath", cursor = ( 2, 0 ) }
                    , { path = "testpath", cursor = ( 0, 0 ) }
                    , { path = "testpath", cursor = ( 4, 0 ) }
                    ]
                , forwards = []
                }
        }
      )
    , ( "GMH<c-o><c-o><tab>?23<cr>"
      , { jumpsCasesBuf
            | jumps =
                { backwards =
                    [ { path = "testpath", cursor = ( 2, 0 ) }
                    , { path = "testpath", cursor = ( 0, 0 ) }
                    , { path = "testpath", cursor = ( 4, 0 ) }
                    ]
                , forwards = []
                }
        }
      )
    , ( "/bc<cr>"
      , { jumpsCasesBuf
            | jumps =
                { backwards = [ { path = "testpath", cursor = ( 0, 0 ) } ]
                , forwards = []
                }
        }
      )
    , ( "G?bc<cr>"
      , { jumpsCasesBuf
            | jumps =
                { backwards =
                    [ { path = "testpath", cursor = ( 4, 0 ) }
                    , { path = "testpath", cursor = ( 0, 0 ) }
                    ]
                , forwards = []
                }
        }
      )
    ]


editBufferCasesBuf : Buffer
editBufferCasesBuf =
    { emptyBuffer
        | name = "test.elm"
        , buffers =
            Dict.fromList
                [ ( "src/test.elm"
                  , { path = "src/test.elm"
                    , version = 0
                    , content = Just ( B.fromString "123", Array.empty )
                    , cursor = ( 0, 0 )
                    }
                  )
                ]
    }


editBufferCases : List ( String, Buffer )
editBufferCases =
    [ ( ":e src/test.elm<cr>"
      , { editBufferCasesBuf
            | jumps =
                { backwards = [ { path = "", cursor = ( 0, 0 ) } ]
                , forwards = []
                }
            , path = "src/test.elm"
            , lines = B.fromString "123"
            , registers =
                Dict.fromList
                    [ ( "#", Text "" )
                    , ( "%", Text "src/test.elm" )
                    ]
            , buffers =
                Dict.fromList
                    [ ( ""
                      , { path = ""
                        , version = 0
                        , content = Just ( emptyBuffer.lines, emptyBuffer.syntax )
                        , cursor = ( 0, 0 )
                        }
                      )
                    ]
        }
      )
    ]


allCases :
    List
        { cases : List ( String, Buffer )
        , map : Buffer -> Buffer
        , model : Buffer
        , name : String
        }
allCases =
    let
        defaultMap buf =
            let
                history =
                    buf.history
            in
                { buf
                    | dotRegister = ""
                    , history = { history | version = 0 }
                    , syntaxDirtyFrom = 0
                    , syntax = Array.empty
                }

        clearSyntax buf =
            { buf
                | syntaxDirtyFrom = 0
                , syntax = Array.empty
            }

        clearScrollTop buf =
            let
                view =
                    buf.view
            in
                { buf | view = { view | scrollTop = 0 } }

        clearJumps buf =
            { buf | jumps = emptyBuffer.jumps }
    in
        [ { name = "ex mode cases"
          , cases = exModeCases
          , model = exModeCasesBuf
          , map =
                (\buf ->
                    let
                        view =
                            buf.view
                    in
                        case buf.mode of
                            Ex ({ prefix, exbuf } as ex) ->
                                { buf
                                    | mode =
                                        Ex
                                            { ex
                                                | prefix = prefix
                                                , exbuf =
                                                    exbuf
                                                        |> Buf.clearHistory
                                                        |> clearSyntax
                                            }
                                }

                            _ ->
                                clearScrollTop buf
                )
                    >> clearJumps
                    >> defaultMap
          }
        , { name = "visual mode cases"
          , cases = visualModeCases
          , model = visualModeCasesBuf
          , map =
                clearScrollTop
                    >> Buf.clearHistory
                    >> clearSyntax
          }
        , { name = "join cases"
          , cases = joinCases
          , model = joinCasesBuf
          , map =
                (\buf -> { buf | last = emptyLast })
                    >> clearScrollTop
                    >> Buf.clearHistory
                    >> defaultMap
          }
        , { name = "jumps cases"
          , cases = jumpsCases
          , model = jumpsCasesBuf
          , map =
                (\buf ->
                    { buf
                        | last = emptyLast
                        , cursor = ( 0, 0 )
                        , cursorColumn = 0
                    }
                )
                    >> defaultMap
          }
        , { name = "edit buffer cases"
          , cases = editBufferCases
          , model = editBufferCasesBuf
          , map =
                (\buf ->
                    { buf
                        | config = emptyBuffer.config
                        , last = emptyBuffer.last
                    }
                )
                    >> defaultMap
          }
        ]


keysTest : (Buffer -> a) -> String -> Buffer -> Buffer -> Test
keysTest map s buf model =
    s
        |> P.run keysParser
        |> Result.map
            (\keys ->
                let
                    buf1 =
                        handleKeys keys model
                in
                    test s <|
                        \_ ->
                            Expect.equal (map buf) (map buf1)
            )
        |> Result.withDefault
            (test s <| \_ -> Expect.fail "invalid input")


clearASTCache : Buffer -> Buffer
clearASTCache buf =
    { buf | vimASTCache = Dict.empty }


suite : Test
suite =
    describe "Press keys" <|
        List.map
            (\{ name, cases, model, map } ->
                describe name <|
                    List.map
                        (\( s, buf ) ->
                            --if s == "llF1$;" then
                            keysTest
                                (clearASTCache >> map)
                                s
                                buf
                                model
                         --else
                         --test s <| (\_ -> Expect.equal 1 1)
                        )
                        cases
            )
            allCases
