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
    P.keep (P.Exactly 1) (always True)
        |> P.andThen
            (\s ->
                case s of
                    "\\" ->
                        P.keep (P.Exactly 1) (\c -> c == '\\' || c == '<')

                    "<" ->
                        P.ignoreUntil ">"
                            |> P.source
                            |> P.map ((++) "<")

                    _ ->
                        P.succeed s
            )
        |> P.repeat P.zeroOrMore


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
                , operator = Nothing
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
                , operator = Nothing
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
                , operator = Nothing
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
                , operator = Nothing
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
                , operator = Nothing
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
                , operator = Nothing
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
                , operator = Nothing
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
                , operator = Nothing
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
                , operator = Nothing
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
                , operator = Nothing
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
        }
      )
    , ( "cw<c-r>\""
      , { putCasesBuf
            | cursor = ( 0, 3 )
            , cursorColumn = 3
            , lines = B.fromString "123\n"
            , registers = Dict.fromList [ ( "\"", "123" ) ]
            , mode = Insert
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
    [ { name = "insert cases"
      , cases = insertCases
      , model = emptyBuffer
      , map = identity
      }
    , { name = "motion cases"
      , cases = motionCases
      , model = motionCasesBuf
      , map = (\buf -> { buf | cursorColumn = 0 })
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
      }
    , { name = "delete cases"
      , cases = deleteCases
      , model = deleteCasesBuf
      , map = (\buf -> { buf | history = emptyBufferHistory })
      }
    , { name = "change cases"
      , cases = changeCases
      , model = changeCasesBuf
      , map =
            (\buf ->
                { buf
                    | history = emptyBufferHistory
                    , continuation = ""
                    , mode = Insert
                }
            )
      }
    , { name = "put cases"
      , cases = putCases
      , model = putCasesBuf
      , map =
            (\buf ->
                { buf
                    | history = emptyBufferHistory
                    , continuation = ""
                }
            )
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
