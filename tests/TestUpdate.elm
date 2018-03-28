module TestUpdate exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Update exposing (update)
import Model exposing (..)
import Buffer as Buf
import Internal.TextBuffer as B exposing (Patch(..))
import Message exposing (Msg(..), Key)
import Parser as P exposing ((|.), (|=), Parser)
import Dict
import Maybe
import Vim.Helper exposing (keyParser)


handleKeys : List Key -> Model -> Model
handleKeys keys model =
    List.foldl
        (\msg model ->
            update msg model
                |> Tuple.first
        )
        model
        (List.map (PressKey 0) keys)


keysParser : Parser (List String)
keysParser =
    P.repeat P.zeroOrMore keyParser


insertCases : List ( String, Buffer )
insertCases =
    [ ( "i"
      , { emptyBuffer
            | mode = Insert
            , continuation = "i"
        }
      )
    , ( "i12"
      , { emptyBuffer
            | cursor = ( 0, 2 )
            , lines = B.fromString "12\n"
            , mode = Insert
            , continuation = "i"
            , history =
                { emptyBufferHistory
                    | pending =
                        Just
                            { cursor = ( 0, 0 )
                            , patches =
                                [ Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                            }
                }
            , last = { emptyLast | inserts = "12" }
        }
      )
    , ( "i12<esc>"
      , { emptyBuffer
            | cursor = ( 0, 1 )
            , cursorColumn = 1
            , lines = B.fromString "12\n"
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                          }
                        ]
                }
            , last = { emptyLast | inserts = "12" }
        }
      )
    , ( "i12<cr><esc>"
      , { emptyBuffer
            | cursor = ( 1, 0 )
            , lines = B.fromString "12\n\n"
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 2 ) ( 1, 0 )
                                , Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                          }
                        ]
                }
            , view =
                let
                    view =
                        emptyBuffer.view
                in
                    { view | scrollTop = 1 }
            , last = { emptyLast | inserts = "12<cr>" }
        }
      )
    , ( "i12<tab><tab><esc>"
      , { emptyBuffer
            | cursor = ( 0, 7 )
            , cursorColumn = 7
            , lines = B.fromString "12      \n"
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 4 ) ( 0, 8 )
                                , Deletion ( 0, 2 ) ( 0, 4 )
                                , Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                          }
                        ]
                }
            , last = { emptyLast | inserts = "12<tab><tab>" }
        }
      )
    , ( "i1<backspace>"
      , { emptyBuffer
            | cursor = ( 0, 0 )
            , mode = Insert
            , continuation = "i"
            , history =
                { emptyBufferHistory
                    | pending =
                        Just
                            { cursor = ( 0, 0 )
                            , patches =
                                [ Insertion ( 0, 0 ) <| B.fromString "1"
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                            }
                }
            , last = { emptyLast | inserts = "1<backspace>" }
        }
      )
    , ( "i1<cr><backspace>"
      , { emptyBuffer
            | cursor = ( 0, 1 )
            , lines = B.fromString "1\n"
            , mode = Insert
            , continuation = "i"
            , history =
                { emptyBufferHistory
                    | pending =
                        Just
                            { cursor = ( 0, 0 )
                            , patches =
                                [ Insertion ( 0, 1 ) <| B.fromString "\n"
                                , Deletion ( 0, 1 ) ( 1, 0 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                            }
                }
            , last = { emptyLast | inserts = "1<cr><backspace>" }
        }
      )
    , ( "i12<esc>u"
      , { emptyBuffer
            | history =
                { emptyBufferHistory
                    | redoes =
                        [ { cursor = ( 0, 1 )
                          , patches =
                                [ Insertion ( 0, 0 ) <| B.fromString "1"
                                , Insertion ( 0, 1 ) <| B.fromString "2"
                                ]
                          }
                        ]
                }
            , last = { emptyLast | inserts = "12" }
        }
      )
    , ( "i12<esc>u<c-r>"
      , { emptyBuffer
            | lines = B.fromString "12\n"
            , cursor = ( 0, 1 )
            , cursorColumn = 1
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                          }
                        ]
                }
            , last = { emptyLast | inserts = "12" }
        }
      )
    , ( "i1<esc>o1"
      , { emptyBuffer
            | lines = B.fromString "1\n1\n"
            , cursor = ( 1, 1 )
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 0 ) ( 0, 1 ) ]
                          }
                        ]
                    , pending =
                        Just
                            { cursor = ( 0, 0 )
                            , patches =
                                [ Deletion ( 1, 0 ) ( 1, 1 )
                                , Deletion ( 1, 0 ) ( 2, 0 )
                                ]
                            }
                }
            , view =
                let
                    view =
                        emptyBuffer.view
                in
                    { view | scrollTop = 1 }
            , mode = Insert
            , continuation = "o"
            , last = { emptyLast | inserts = "1" }
        }
      )
    , ( "i1<esc>O2"
      , { emptyBuffer
            | lines = B.fromString "2\n1\n"
            , cursor = ( 0, 1 )
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 0 ) ( 0, 1 ) ]
                          }
                        ]
                    , pending =
                        Just
                            { cursor = ( 0, 0 )
                            , patches =
                                [ Deletion ( 0, 0 ) ( 0, 1 )
                                , Deletion ( 0, 0 ) ( 1, 0 )
                                ]
                            }
                }
            , mode = Insert
            , continuation = "O"
            , last = { emptyLast | inserts = "2" }
        }
      )
    ]


motionCasesBuf : Buffer
motionCasesBuf =
    { emptyBuffer
        | lines =
            B.fromString
                """123
45678
"""
    }


motionCases : List ( String, Buffer )
motionCases =
    [ ( "l"
      , { motionCasesBuf | cursor = ( 0, 1 ) }
      )
    , ( "lh"
      , { motionCasesBuf | cursor = ( 0, 0 ) }
      )
    , ( "lll"
      , { motionCasesBuf | cursor = ( 0, 2 ) }
      )
    , ( "hh"
      , { motionCasesBuf | cursor = ( 0, 0 ) }
      )
    , ( "j"
      , { motionCasesBuf
            | cursor = ( 1, 0 )
            , view =
                let
                    view =
                        emptyBuffer.view
                in
                    { view | scrollTop = 1 }
        }
      )
    , ( "jk"
      , { motionCasesBuf | cursor = ( 0, 0 ) }
      )
    , ( "jlllllllk"
      , { motionCasesBuf | cursor = ( 0, 2 ) }
      )
    , ( "e"
      , { motionCasesBuf | cursor = ( 0, 2 ) }
      )
    , ( "w"
      , { motionCasesBuf
            | cursor = ( 1, 0 )
            , view =
                let
                    view =
                        emptyBuffer.view
                in
                    { view | scrollTop = 1 }
        }
      )
    , ( "f3"
      , { motionCasesBuf
            | cursor = ( 0, 2 )
            , last =
                { matchChar =
                    Just
                        { char = "3"
                        , before = False
                        , forward = True
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "f4"
      , { motionCasesBuf
            | cursor = ( 0, 0 )
            , last =
                { matchChar =
                    Just
                        { char = "4"
                        , before = False
                        , forward = True
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "t3"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , last =
                { matchChar =
                    Just
                        { char = "3"
                        , before = True
                        , forward = True
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "t3F1"
      , { motionCasesBuf
            | cursor = ( 0, 0 )
            , last =
                { matchChar =
                    Just
                        { char = "1"
                        , before = False
                        , forward = False
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "f3T1"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , last =
                { matchChar =
                    Just
                        { char = "1"
                        , before = True
                        , forward = False
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "f3hh;"
      , { motionCasesBuf
            | cursor = ( 0, 2 )
            , last =
                { matchChar =
                    Just
                        { char = "3"
                        , before = False
                        , forward = True
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "t30;"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , last =
                { matchChar =
                    Just
                        { char = "3"
                        , before = True
                        , forward = True
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "f1$,"
      , { motionCasesBuf
            | cursor = ( 0, 0 )
            , last =
                { matchChar =
                    Just
                        { char = "1"
                        , before = False
                        , forward = True
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "t1$,"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , last =
                { matchChar =
                    Just
                        { char = "1"
                        , before = True
                        , forward = True
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    , ( "llF1$;"
      , { motionCasesBuf
            | cursor = ( 0, 0 )
            , last =
                { matchChar =
                    Just
                        { char = "1"
                        , before = False
                        , forward = False
                        }
                , matchString = Nothing
                , inserts = ""
                }
        }
      )
    ]


exModeCases : List ( String, Buffer )
exModeCases =
    [ ( ":11"
      , { emptyBuffer
            | mode =
                Ex ":"
                    { emptyExBuffer
                        | lines = B.fromString ":11"
                        , cursor = ( 0, 3 )
                    }
            , continuation = ":"
        }
      )
    , ( "/1<backspace><backspace>", emptyBuffer )
    , ( "/11<cr>", emptyBuffer )
    ]


deleteCasesBuf : Buffer
deleteCasesBuf =
    { emptyBuffer
        | lines = B.fromString """ 123
456
"""
    }


deleteCases : List ( String, Buffer )
deleteCases =
    [ ( "de"
      , { deleteCasesBuf
            | lines = B.fromString "\n456\n"
            , registers = Dict.fromList [ ( "\"", " 123" ) ]
        }
      )
    , ( "dfa", deleteCasesBuf )
    , ( "dw"
      , { deleteCasesBuf
            | lines = B.fromString "123\n456\n"
            , registers = Dict.fromList [ ( "\"", " " ) ]
        }
      )
    , ( "dvw"
      , { deleteCasesBuf
            | lines = B.fromString "23\n456\n"
            , registers = Dict.fromList [ ( "\"", " 1" ) ]
        }
      )
    , ( "ldw"
      , { deleteCasesBuf
            | lines = B.fromString " \n456\n"
            , registers = Dict.fromList [ ( "\"", "123" ) ]
            , cursorColumn = 1
        }
      )
    ]


changeCasesBuf : Buffer
changeCasesBuf =
    { emptyBuffer
        | lines = B.fromString """ 123
456
"""
    }


changeCases : List ( String, Buffer )
changeCases =
    [ ( "ce"
      , { changeCasesBuf
            | lines = B.fromString "\n456\n"
            , registers = Dict.fromList [ ( "\"", " 123" ) ]
        }
      )
    , ( "cfa", changeCasesBuf )
    , ( "cw"
      , { changeCasesBuf
            | lines = B.fromString "123\n456\n"
            , registers = Dict.fromList [ ( "\"", " " ) ]
        }
      )
    , ( "cvw"
      , { changeCasesBuf
            | lines = B.fromString "23\n456\n"
            , registers = Dict.fromList [ ( "\"", " 1" ) ]
        }
      )
    ]


putCasesBuf : Buffer
putCasesBuf =
    { emptyBuffer
        | lines = B.fromString "123\n"
        , registers = Dict.fromList [ ( "\"", "abc" ) ]
    }


putCases : List ( String, Buffer )
putCases =
    [ ( "p"
      , { putCasesBuf
            | lines = B.fromString "1abc23\n"
            , cursor = ( 0, 3 )
            , cursorColumn = 3
        }
      )
    , ( "P"
      , { putCasesBuf
            | lines = B.fromString "abc123\n"
            , cursor = ( 0, 2 )
            , cursorColumn = 2
        }
      )
    , ( "dwp"
      , { putCasesBuf
            | cursor = ( 0, 2 )
            , cursorColumn = 2
            , registers = Dict.fromList [ ( "\"", "123" ) ]
        }
      )
    , ( "dwP"
      , { putCasesBuf
            | cursor = ( 0, 2 )
            , cursorColumn = 2
            , registers = Dict.fromList [ ( "\"", "123" ) ]
        }
      )
    , ( "i<c-r>\""
      , { putCasesBuf
            | cursor = ( 0, 3 )
            , cursorColumn = 3
            , lines = B.fromString "abc123\n"
            , mode = Insert
            , last = { emptyLast | inserts = "<c-r>\"" }
        }
      )
    , ( "cw<c-r>\""
      , { putCasesBuf
            | cursor = ( 0, 3 )
            , cursorColumn = 3
            , lines = B.fromString "123\n"
            , registers = Dict.fromList [ ( "\"", "123" ) ]
            , mode = Insert
            , last = { emptyLast | inserts = "<c-r>\"" }
        }
      )
    ]


dotRepeatCasesBuf : Buffer
dotRepeatCasesBuf =
    { emptyBuffer | lines = B.fromString "123\n456\n" }


emptyLast :
    { matchChar : Maybe a
    , matchString : Maybe a1
    , inserts : String
    }
emptyLast =
    { matchChar = Nothing
    , matchString = Nothing
    , inserts = ""
    }


dotRepeatCases : List ( String, Buffer )
dotRepeatCases =
    [ ( "iaa"
      , { dotRepeatCasesBuf
            | lines = B.fromString "aa123\n456\n"
            , cursor = ( 0, 2 )
            , last = { emptyLast | inserts = "aa" }
        }
      )
    , ( "iaa<esc>"
      , { dotRepeatCasesBuf
            | registers =
                Dict.fromList [ ( ".", "i<inserts><esc>" ) ]
            , lines = B.fromString "aa123\n456\n"
            , cursor = ( 0, 1 )
            , last = { emptyLast | inserts = "aa" }
        }
      )
    , ( "dw"
      , { dotRepeatCasesBuf
            | lines = B.fromString "\n456\n"
            , registers =
                Dict.fromList [ ( ".", "dw" ) ]
        }
      )
    , ( "dl."
      , { dotRepeatCasesBuf
            | lines = B.fromString "3\n456\n"
            , registers =
                Dict.fromList [ ( ".", "dl" ) ]
        }
      )
    , ( "iaa<esc>."
      , { dotRepeatCasesBuf
            | registers =
                Dict.fromList [ ( ".", "i<inserts><esc>" ) ]
            , lines = B.fromString "aaaa123\n456\n"
            , cursor = ( 0, 2 )
            , last = { emptyLast | inserts = "aa" }
        }
      )
    , ( "iaa<c-o>hbb<esc>."
      , { dotRepeatCasesBuf
            | registers =
                Dict.fromList [ ( ".", "i<inserts><esc>" ) ]
            , lines = B.fromString "abbbba123\n456\n"
            , cursor = ( 0, 3 )
            , last = { emptyLast | inserts = "bb" }
        }
      )
    ]


visualModeCasesBuf : Buffer
visualModeCasesBuf =
    { emptyBuffer | lines = B.fromString "123\n456\n" }


visualModeCases : List ( String, Buffer )
visualModeCases =
    [ ( "v"
      , { visualModeCasesBuf
            | mode = Visual VisualRange ( 0, 0 ) ( 0, 0 )
        }
      )
    , ( "V"
      , { visualModeCasesBuf
            | mode = Visual VisualLine ( 0, 0 ) ( 0, 0 )
        }
      )
    , ( "<c-v>"
      , { visualModeCasesBuf
            | mode = Visual VisualBlock ( 0, 0 ) ( 0, 0 )
        }
      )
    , ( "vv"
      , visualModeCasesBuf
      )
    , ( "vw"
      , { visualModeCasesBuf
            | mode = Visual VisualRange ( 0, 0 ) ( 1, 0 )
            , cursor = ( 1, 0 )
        }
      )
    , ( "vwl"
      , { visualModeCasesBuf
            | mode = Visual VisualRange ( 0, 0 ) ( 1, 1 )
            , cursor = ( 1, 1 )
            , cursorColumn = 1
        }
      )
    , ( "lvh"
      , { visualModeCasesBuf
            | mode = Visual VisualRange ( 0, 1 ) ( 0, 0 )
        }
      )
    , ( "vlo"
      , { visualModeCasesBuf
            | mode = Visual VisualRange ( 0, 1 ) ( 0, 0 )
        }
      )
    ]


allCases :
    List
        { cases : List ( String, Buffer )
        , map : Buffer -> Buffer
        , model : Model
        , name : String
        }
allCases =
    let
        defaultMap =
            Buf.setRegister "." ""
    in
        [ { name = "insert cases"
          , cases = insertCases
          , model = emptyBuffer
          , map = defaultMap
          }
        , { name = "motion cases"
          , cases = motionCases
          , model = motionCasesBuf
          , map =
                (\buf -> { buf | cursorColumn = 0 })
                    >> defaultMap
          }
        , { name = "ex mode cases"
          , cases = exModeCases
          , model = emptyBuffer
          , map =
                (\buf ->
                    case buf.mode of
                        Ex prefix exBuf ->
                            { buf
                                | mode =
                                    Ex prefix (Buf.clearHistory exBuf)
                            }

                        _ ->
                            buf
                )
                    >> defaultMap
          }
        , { name = "delete cases"
          , cases = deleteCases
          , model = deleteCasesBuf
          , map = Buf.clearHistory >> defaultMap
          }
        , { name = "change cases"
          , cases = changeCases
          , model = changeCasesBuf
          , map =
                (\buf ->
                    { buf
                        | continuation = ""
                        , mode = Insert
                    }
                )
                    >> Buf.clearHistory
                    >> defaultMap
          }
        , { name = "put cases"
          , cases = putCases
          , model = putCasesBuf
          , map =
                (\buf -> { buf | continuation = "" })
                    >> Buf.clearHistory
                    >> defaultMap
          }
        , { name = "dot repeat cases"
          , cases = dotRepeatCases
          , model = dotRepeatCasesBuf
          , map =
                (\buf ->
                    { emptyBuffer
                        | registers =
                            buf.registers
                                |> Dict.get "."
                                |> Maybe.map
                                    (\s ->
                                        Dict.fromList
                                            [ ( "."
                                              , s
                                              )
                                            ]
                                    )
                                |> Maybe.withDefault Dict.empty
                        , last = buf.last
                        , lines = buf.lines
                        , cursor = buf.cursor
                    }
                )
                    >> Buf.clearHistory
          }
        , { name = "visual mode cases"
          , cases = visualModeCases
          , model = visualModeCasesBuf
          , map =
                (\buf ->
                    let
                        view =
                            buf.view
                    in
                        { buf
                            | continuation = ""
                            , view = { view | scrollTop = 0 }
                        }
                )
                    >> Buf.clearHistory
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


suite : Test
suite =
    describe "Press keys" <|
        List.map
            (\{ name, cases, model, map } ->
                describe name <|
                    List.map
                        (\( s, buf ) ->
                            keysTest
                                map
                                s
                                buf
                                model
                        )
                        cases
            )
            allCases
