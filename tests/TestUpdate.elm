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
import Vim.AST exposing (VisualType(..))
import Elm.Array as Array
import Jumps exposing (Location)


handleKeys : List Key -> Model -> Model
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
                    | pending = [ Deletion ( 0, 0 ) ( 0, 2 ) ]
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
                        [ [ Deletion ( 0, 0 ) ( 0, 2 ) ] ]
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
                        [ [ Deletion ( 0, 0 ) ( 1, 0 ) ] ]
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
            | cursor = ( 0, 5 )
            , cursorColumn = 5
            , lines = B.fromString "12    \n"
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ [ Deletion ( 0, 0 ) ( 0, 6 ) ] ]
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
                        [ Insertion ( 0, 0 ) <| B.fromString "1"
                        , Deletion ( 0, 0 ) ( 0, 1 )
                        ]
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
                        [ Insertion ( 0, 1 ) <| B.fromString "\n"
                        , Deletion ( 0, 0 ) ( 1, 0 )
                        ]
                }
            , last = { emptyLast | inserts = "1<cr><backspace>" }
        }
      )
    , ( "i12<esc>u"
      , { emptyBuffer
            | history =
                { emptyBufferHistory
                    | redoes =
                        [ [ Insertion ( 0, 0 ) <| B.fromString "12"
                          ]
                        ]
                }
            , last = { emptyLast | inserts = "12" }
        }
      )
    , ( "i12<esc>u<c-r>"
      , { emptyBuffer
            | lines = B.fromString "12\n"
            , cursor = ( 0, 0 )
            , cursorColumn = 0
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ [ Deletion ( 0, 0 ) ( 0, 2 ) ] ]
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
                        [ [ Deletion ( 0, 0 ) ( 0, 1 ) ] ]
                    , pending =
                        [ Deletion ( 1, 0 ) ( 1, 1 ), Deletion ( 1, 0 ) ( 2, 0 ) ]
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
                        [ [ Deletion ( 0, 0 ) ( 0, 1 ) ]
                        ]
                    , pending =
                        [ Deletion ( 0, 0 ) ( 0, 1 )
                        , Deletion ( 0, 0 ) ( 1, 0 )
                        ]
                }
            , mode = Insert
            , continuation = "O"
            , last = { emptyLast | inserts = "2" }
        }
      )
    , ( "i<cr><esc>"
      , { emptyBuffer
            | lines = B.fromString "\n\n"
            , last = { emptyLast | inserts = "<cr>" }
            , cursor = ( 1, 0 )
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ [ Deletion ( 0, 0 ) ( 1, 0 ) ]
                        ]
                }
        }
      )
    , ( "i123<cr><esc>kC"
      , { emptyBuffer
            | lines = B.fromString "\n\n"
            , mode = Insert
            , continuation = "C"
            , registers = Dict.fromList [ ( "\"", Text "123" ) ]
            , history =
                { undoes =
                    [ [ Deletion ( 0, 0 ) ( 1, 0 )
                      ]
                    ]
                , pending =
                    [ Insertion ( 0, 0 )
                        (B.fromString "123")
                    ]
                , redoes = []
                , savePoint = 0
                , version = 0
                }
        }
      )
    ]


motionCasesBuf : Buffer
motionCasesBuf =
    let
        view =
            emptyBuffer.view
    in
        { emptyBuffer
            | lines =
                B.fromString
                    """123
45678
"""

            --, view = { view | size = { width = 20, height = 6 } }
        }


motionCases : List ( String, Buffer )
motionCases =
    [ ( "l"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , cursorColumn = 1
        }
      )
    , ( "lh"
      , { motionCasesBuf | cursor = ( 0, 0 ) }
      )
    , ( "lll"
      , { motionCasesBuf
            | cursor = ( 0, 2 )
            , cursorColumn = 2
        }
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
      , { motionCasesBuf
            | cursor = ( 0, 2 )
            , cursorColumn = 4
        }
      )
    , ( "e"
      , { motionCasesBuf
            | cursor = ( 0, 2 )
            , cursorColumn = 2
        }
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
            , cursorColumn = 2
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "3"
                            , before = False
                            , forward = True
                            }
                }
        }
      )
    , ( "f4"
      , { motionCasesBuf
            | cursor = ( 0, 0 )
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "4"
                            , before = False
                            , forward = True
                            }
                }
        }
      )
    , ( "t3"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , cursorColumn = 1
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "3"
                            , before = True
                            , forward = True
                            }
                }
        }
      )
    , ( "t3F1"
      , { motionCasesBuf
            | cursor = ( 0, 0 )
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "1"
                            , before = False
                            , forward = False
                            }
                }
        }
      )
    , ( "f3T1"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , cursorColumn = 1
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "1"
                            , before = True
                            , forward = False
                            }
                }
        }
      )
    , ( "f3hh;"
      , { motionCasesBuf
            | cursor = ( 0, 2 )
            , cursorColumn = 2
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "3"
                            , before = False
                            , forward = True
                            }
                }
        }
      )
    , ( "t30;"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , cursorColumn = 1
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "3"
                            , before = True
                            , forward = True
                            }
                }
        }
      )
    , ( "f1$,"
      , { motionCasesBuf
            | cursor = ( 0, 0 )
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "1"
                            , before = False
                            , forward = True
                            }
                }
        }
      )
    , ( "t1$,"
      , { motionCasesBuf
            | cursor = ( 0, 1 )
            , cursorColumn = 1
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "1"
                            , before = True
                            , forward = True
                            }
                }
        }
      )
    , ( "llF1$;"
      , { motionCasesBuf
            | cursor = ( 0, 0 )
            , last =
                { emptyLast
                    | matchChar =
                        Just
                            { char = "1"
                            , before = False
                            , forward = False
                            }
                }
        }
      )
    , ( "VGdiaa<esc>"
      , { motionCasesBuf
            | lines = B.fromString "aa"
            , cursor = ( 0, 1 )
            , cursorColumn = 1
            , last =
                { emptyLast
                    | inserts = "aa"
                    , visual = "G"
                }
        }
      )
    , ( "jek"
      , { motionCasesBuf
            | cursorColumn = 4
            , cursor = ( 0, 2 )
        }
      )
    ]


exModeCasesBuf : Buffer
exModeCasesBuf =
    { emptyBuffer | lines = B.fromString "abc\ndef\ndef\n" }


exModeCases : List ( String, Buffer )
exModeCases =
    [ ( ":11"
      , { exModeCasesBuf
            | mode =
                Ex
                    { prefix = ExCommand
                    , exbuf =
                        { emptyExBuffer
                            | lines = B.fromString ":11"
                            , cursor = ( 0, 3 )
                        }
                    , visual = Nothing
                    , autoComplete = Nothing
                    }
            , continuation = ":"
            , last = { emptyLast | ex = "11" }
        }
      )
    , ( ":11<backspace>"
      , { exModeCasesBuf
            | mode =
                Ex
                    { prefix = ExCommand
                    , exbuf =
                        { emptyExBuffer
                            | lines = B.fromString ":1"
                            , cursor = ( 0, 2 )
                        }
                    , visual = Nothing
                    , autoComplete = Nothing
                    }
            , continuation = ":"
            , last = { emptyLast | ex = "11<backspace>" }
        }
      )
    , ( "/1<backspace><backspace>"
      , { exModeCasesBuf
            | last = { emptyLast | ex = "1<backspace>" }
        }
      )
    , ( "/11<cr>"
      , { exModeCasesBuf
            | last =
                { emptyLast
                    | matchString = Just ( "11", True )
                    , ex = "11"
                }
        }
      )
    , ( "/bc<cr>"
      , { exModeCasesBuf
            | cursor = ( 0, 1 )
            , cursorColumn = 1
            , last =
                { emptyLast
                    | matchString = Just ( "bc", True )
                    , ex = "bc"
                }
        }
      )
    , ( "/de<cr>"
      , { exModeCasesBuf
            | cursor = ( 1, 0 )
            , cursorColumn = 0
            , last =
                { emptyLast
                    | matchString = Just ( "de", True )
                    , ex = "de"
                }
        }
      )
    , ( "?123<cr>"
      , { exModeCasesBuf
            | last =
                { emptyLast
                    | matchString = Just ( "123", False )
                    , ex = "123"
                }
        }
      )
    , ( "/de<cr>?ab<cr>"
      , { exModeCasesBuf
            | last =
                { emptyLast
                    | matchString = Just ( "ab", False )
                    , ex = "ab"
                }
        }
      )
    , ( "jj?def<cr>"
      , { exModeCasesBuf
            | last = { emptyLast | matchString = Just ( "def", False ) }
            , cursor = ( 1, 0 )
            , last =
                { emptyLast
                    | ex = "def"
                    , matchString = Just ( "def", False )
                }
        }
      )
    , ( "/"
      , { exModeCasesBuf
            | mode =
                Ex
                    { exbuf =
                        { emptyExBuffer
                            | lines = B.fromString "/"
                            , cursor = ( 0, 1 )
                        }
                    , visual = Nothing
                    , prefix =
                        ExSearch
                            { match = Nothing
                            , forward = True
                            }
                    , autoComplete = Nothing
                    }
            , continuation = "/"
        }
      )
    , ( "?"
      , { exModeCasesBuf
            | mode =
                Ex
                    { exbuf =
                        { emptyExBuffer
                            | lines = B.fromString "?"
                            , cursor = ( 0, 1 )
                        }
                    , visual = Nothing
                    , prefix =
                        ExSearch
                            { match = Nothing
                            , forward = False
                            }
                    , autoComplete = Nothing
                    }
            , continuation = "?"
        }
      )
    , ( "v/ef<cr>"
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
            , registers = Dict.fromList [ ( "\"", Text " 123" ) ]
        }
      )
    , ( "dfa", deleteCasesBuf )
    , ( "dw"
      , { deleteCasesBuf
            | lines = B.fromString "123\n456\n"
            , registers = Dict.fromList [ ( "\"", Text " " ) ]
        }
      )
    , ( "dvw"
      , { deleteCasesBuf
            | lines = B.fromString "23\n456\n"
            , registers = Dict.fromList [ ( "\"", Text " 1" ) ]
        }
      )
    , ( "ldw"
      , { deleteCasesBuf
            | lines = B.fromString " \n456\n"
            , registers = Dict.fromList [ ( "\"", Text "123" ) ]
            , cursorColumn = 0
        }
      )
    , ( "Vjd"
      , { deleteCasesBuf
            | lines = B.empty
            , registers = Dict.fromList [ ( "\"", Lines " 123\n456\n" ) ]
            , last = { emptyLast | visual = "j" }
        }
      )
    , ( "jVd"
      , { deleteCasesBuf
            | lines = B.fromString " 123\n"
            , registers = Dict.fromList [ ( "\"", Lines "456\n" ) ]
            , cursor = ( 1, 0 )
        }
      )
    , ( "d/456<cr>"
      , { deleteCasesBuf
            | lines = B.fromString "456\n"
            , registers = Dict.fromList [ ( "\"", Text " 123\n" ) ]
            , last =
                { emptyLast
                    | ex = "456"
                    , matchString = Just ( "456", True )
                }
        }
      )
    , ( "dv/456<cr>"
      , { deleteCasesBuf
            | lines = B.fromString "56\n"
            , registers = Dict.fromList [ ( "\"", Text " 123\n4" ) ]
            , last =
                { emptyLast
                    | ex = "456"
                    , matchString = Just ( "456", True )
                }
        }
      )
    ]


changeCasesBuf : Buffer
changeCasesBuf =
    { emptyBuffer
        | lines = B.fromString """ 123
456
def
def
"""
    }


changeCases : List ( String, Buffer )
changeCases =
    [ ( "ce"
      , { changeCasesBuf
            | lines = B.fromString "\n456\ndef\ndef\n"
            , mode = Insert
            , registers = Dict.fromList [ ( "\"", Text " 123" ) ]
            , continuation = "ce"
        }
      )
    , ( "cfa"
      , { changeCasesBuf
            | mode = Insert
            , continuation = "cfa"
        }
      )
    , ( "cw"
      , { changeCasesBuf
            | lines = B.fromString "123\n456\ndef\ndef\n"
            , mode = Insert
            , registers = Dict.fromList [ ( "\"", Text " " ) ]
            , continuation = "cw"
        }
      )
    , ( "cvw"
      , { changeCasesBuf
            | lines = B.fromString " 123\n456\ndef\ndef\n"
            , mode = Insert
            , registers = Dict.fromList [ ( "\"", Text "" ) ]
            , continuation = "cvw"
        }
      )
    , ( "c/ef<cr>"
      , { changeCasesBuf
            | mode = Insert
            , lines = B.fromString "ef\ndef\n"
            , continuation = "c/<cr>"
            , history =
                { emptyBufferHistory
                    | pending =
                        [ Insertion
                            ( 0, 0 )
                            (B.fromString " 123\n456\nd")
                        ]
                }
            , registers = Dict.fromList [ ( "\"", Text " 123\n456\nd" ) ]
            , last =
                { emptyLast
                    | matchString = Just ( "ef", True )
                    , ex = "ef"
                }
        }
      )
    , ( "c/<esc>", changeCasesBuf )
    ]


replaceCasesBuf : Buffer
replaceCasesBuf =
    { emptyBuffer | lines = B.fromString "123\n  456\n" }


replaceCases : List ( String, Buffer )
replaceCases =
    [ ( "lrb"
      , { replaceCasesBuf
            | lines = B.fromString "1b3\n  456\n"
            , cursor = ( 0, 1 )
            , cursorColumn = 1
        }
      )
    , ( "jlllr<cr>"
      , { replaceCasesBuf
            | lines = B.fromString "123\n  4\n  6\n"
            , cursor = ( 2, 1 )
            , cursorColumn = 1
            , last = { emptyLast | indent = 2 }
        }
      )
    , ( "jllllr<cr>"
      , { replaceCasesBuf
            | lines = B.fromString "123\n  45\n\n"
            , cursor = ( 2, 0 )
            , last = { emptyLast | indent = 2 }
        }
      )
    ]


putCasesBuf : Buffer
putCasesBuf =
    { emptyBuffer
        | lines = B.fromString "123\n456\n"
        , registers =
            Dict.fromList
                [ ( "\"", Text "abc" )
                , ( "a", Lines "newline\n" )
                ]
    }


putCases : List ( String, Buffer )
putCases =
    [ ( "\"ap"
      , { putCasesBuf
            | lines = B.fromString "123\nnewline\n456\n"
            , cursor = ( 1, 0 )
        }
      )
    , ( "\"aP"
      , { putCasesBuf
            | lines = B.fromString "newline\n123\n456\n"
            , cursor = ( 0, 0 )
        }
      )
    , ( "j\"ap"
      , { putCasesBuf
            | lines = B.fromString "123\n456\nnewline\n"
            , cursor = ( 2, 0 )
        }
      )
    , ( "p"
      , { putCasesBuf
            | lines = B.fromString "1abc23\n456\n"
            , cursor = ( 0, 3 )
            , cursorColumn = 3
        }
      )
    , ( "P"
      , { putCasesBuf
            | lines = B.fromString "abc123\n456\n"
            , cursor = ( 0, 2 )
            , cursorColumn = 2
        }
      )
    , ( "dwp"
      , { putCasesBuf
            | cursor = ( 0, 2 )
            , cursorColumn = 2
            , registers =
                Dict.fromList
                    [ ( "\"", Text "123" )
                    , ( "a", Lines "newline\n" )
                    ]
        }
      )
    , ( "dwP"
      , { putCasesBuf
            | cursor = ( 0, 2 )
            , cursorColumn = 2
            , registers =
                Dict.fromList
                    [ ( "\"", Text "123" )
                    , ( "a", Lines "newline\n" )
                    ]
        }
      )
    , ( "i<c-r>\""
      , { putCasesBuf
            | cursor = ( 0, 3 )
            , cursorColumn = 3
            , lines = B.fromString "abc123\n456\n"
            , mode = Insert
            , last = { emptyLast | inserts = "<c-r>\"" }
        }
      )
    , ( "cw<c-r>\""
      , { putCasesBuf
            | cursor = ( 0, 3 )
            , cursorColumn = 3
            , lines = B.fromString "123\n456\n"
            , registers =
                Dict.fromList
                    [ ( "\"", Text "123" )
                    , ( "a", Lines "newline\n" )
                    ]
            , mode = Insert
            , last = { emptyLast | inserts = "<c-r>\"" }
        }
      )
    ]


dotRepeatCasesBuf : Buffer
dotRepeatCasesBuf =
    { emptyBuffer | lines = B.fromString "123\n456\n456\n" }


emptyLast :
    { matchChar : Maybe a
    , matchString : Maybe a1
    , inserts : String
    , visual : String
    , ex : String
    , indent : Int
    , jumpToTag : Maybe Location
    }
emptyLast =
    { matchChar = Nothing
    , matchString = Nothing
    , inserts = ""
    , visual = ""
    , ex = ""
    , indent = 0
    , jumpToTag = Nothing
    }


dotRepeatCases : List ( String, Buffer )
dotRepeatCases =
    [ ( "iaa"
      , { dotRepeatCasesBuf
            | lines = B.fromString "aa123\n456\n456\n"
            , cursor = ( 0, 2 )
            , last = { emptyLast | inserts = "aa" }
        }
      )
    , ( "iaa<esc>"
      , { dotRepeatCasesBuf
            | dotRegister = "i<inserts><esc>"
            , lines = B.fromString "aa123\n456\n456\n"
            , cursor = ( 0, 1 )
            , last = { emptyLast | inserts = "aa" }
        }
      )
    , ( "dw"
      , { dotRepeatCasesBuf
            | lines = B.fromString "\n456\n456\n"
            , dotRegister = "dw"
        }
      )
    , ( "dl."
      , { dotRepeatCasesBuf
            | lines = B.fromString "3\n456\n456\n"
            , dotRegister = "dl"
        }
      )
    , ( "iaa<esc>."
      , { dotRepeatCasesBuf
            | dotRegister = "i<inserts><esc>"
            , lines = B.fromString "aaaa123\n456\n456\n"
            , cursor = ( 0, 2 )
            , last = { emptyLast | inserts = "aa" }
        }
      )
    , ( "iaa<c-o>hbb<esc>."
      , { dotRepeatCasesBuf
            | dotRegister = "i<inserts><esc>"
            , lines = B.fromString "abbbba123\n456\n456\n"
            , cursor = ( 0, 3 )
            , last = { emptyLast | inserts = "bb" }
        }
      )
    , ( "d/4<cr>"
      , { dotRepeatCasesBuf
            | dotRegister = "d/<exbuf><cr>"
            , lines = B.fromString "456\n456\n"
            , last =
                { emptyLast
                    | ex = "4"
                    , matchString = Just ( "4", True )
                }
        }
      )
    , ( "d/4<cr>."
      , { dotRepeatCasesBuf
            | dotRegister = "d/<exbuf><cr>"
            , lines = B.fromString "456\n"
            , last =
                { emptyLast
                    | ex = "4"
                    , matchString = Just ( "4", True )
                }
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
            | mode = Insert
            , lines = B.fromString "23\n456\n"
            , registers = Dict.fromList [ ( "\"", Text "1" ) ]
            , continuation = "vc"
        }
      )
    , ( "vlc"
      , { visualModeCasesBuf
            | mode = Insert
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
            | mode = Normal
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
        | name = "src/test.elm"
        , buffers =
            Dict.fromList
                [ ( "src/test.elm"
                  , { path = "src/test.elm"
                    , content = Just ( B.fromString "123", Array.empty )
                    , cursor = ( 0, 0 )
                    , scrollTop = 0
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
                        , content = Just ( emptyBuffer.lines, emptyBuffer.syntax )
                        , scrollTop = 0
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
        , model : Model
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
        [ { name = "insert cases"
          , cases = insertCases
          , model = emptyBuffer
          , map =
                (\buf -> { buf | view = emptyView })
                    >> defaultMap
          }
        , { name = "motion cases"
          , cases = motionCases
          , model = motionCasesBuf
          , map =
                (\buf ->
                    { buf
                        | history = emptyBufferHistory
                        , registers = Dict.empty
                        , jumps = emptyBuffer.jumps
                    }
                )
                    >> defaultMap
          }
        , { name = "ex mode cases"
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
        , { name = "delete cases"
          , cases = deleteCases
          , model = deleteCasesBuf
          , map =
                (\buf ->
                    let
                        view =
                            buf.view
                    in
                        clearScrollTop buf
                )
                    >> Buf.clearHistory
                    >> defaultMap
          }
        , { name = "change cases"
          , cases = changeCases
          , model = changeCasesBuf
          , map =
                Buf.clearHistory
                    >> defaultMap
          }
        , { name = "put cases"
          , cases = putCases
          , model = putCasesBuf
          , map =
                (\buf -> { buf | continuation = "" })
                    >> clearScrollTop
                    >> Buf.clearHistory
                    >> defaultMap
          }
        , { name = "replace cases"
          , cases = replaceCases
          , model = replaceCasesBuf
          , map =
                (\buf -> { buf | continuation = "" })
                    >> clearScrollTop
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
                                    (\s -> Dict.fromList [ ( ".", s ) ])
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
