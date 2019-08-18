module BufferTest exposing (repeat, suite)

import Array as Array
import Expect exposing (Expectation)
import Fuzz
import Internal.Brackets exposing (pairBracketAt)
import Internal.Syntax exposing (TokenType(..), iterateTokens)
import Internal.TextBuffer as B exposing (Patch(..))
import Model.Buffer exposing (..)
import Model.BufferHistory exposing (..)
import Test exposing (..)
import TextBuffer exposing (..)
import Update.Buffer exposing (..)


repeat : Int -> (a -> a) -> (a -> a)
repeat n f =
    let
        helper n_ x =
            case max n_ 0 of
                0 ->
                    x

                m ->
                    helper (m - 1) (f x)
    in
    helper n


suite : Test
suite =
    --    describe "debug" <|
    --        [ test "debug undo" <|
    --            (\_ ->
    --                let
    --                    patcheslist =
    --                        [ [ Insertion ( 0, 1 ) <| B.fromString " "
    --                          , Insertion ( 0, 0 ) <| B.fromString "\n"
    --                          ]
    --                        ]
    --
    --                    buf =
    --                        List.foldl
    --                            (\patches buf ->
    --                                buf
    --                                    |> transaction patches
    --                                    |> commit
    --                            )
    --                            emptyBuffer
    --                            patcheslist
    --
    --                    buf1 =
    --                        List.foldl (\_ b -> undo b) buf patcheslist
    --                in
    --                    Expect.equal
    --                        emptyBuffer.lines
    --                        buf1.lines
    --            )
    --        ]
    describe "basic cases"
        [ describe "insert" <|
            let
                buf =
                    transaction
                        [ Insertion ( 0, 0 ) <|
                            B.fromString "123"
                        ]
                        emptyBuffer

                history =
                    buf.history
            in
            [ test "result.view.cursor" <|
                \_ ->
                    Expect.equal ( 0, 3 ) buf.view.cursor
            , test "result.lines" <|
                \_ ->
                    Expect.equal (B.fromString "123\n")
                        buf.lines
            , test "result.history" <|
                \_ ->
                    Expect.equal
                        { emptyBufferHistory
                            | undoes = []
                            , pending =
                                { patches = [ Deletion ( 0, 0 ) ( 0, 3 ) ]
                                , cursor = ( 0, 0 )
                                }
                            , redoes = []
                            , version = 1
                            , changes = []
                            , pendingChanges =
                                [ Insertion ( 0, 0 ) <|
                                    B.fromString "123"
                                ]
                        }
                        { history | diff = [] }
            ]
        , describe "insert patches" <|
            let
                buf =
                    transaction
                        [ Insertion ( 0, 0 ) <| B.fromString "123\n"
                        , Insertion ( 1, 0 ) <| B.fromString "123"
                        ]
                        emptyBuffer

                history =
                    buf.history
            in
            [ test "result.view.cursor" <|
                \_ ->
                    Expect.equal ( 1, 3 ) buf.view.cursor
            , test "result.lines" <|
                \_ ->
                    Expect.equal (B.fromString "123\n123\n")
                        buf.lines
            , test "result.history" <|
                \_ ->
                    Expect.equal
                        { emptyBufferHistory
                            | undoes = []
                            , pending =
                                { patches = [ Deletion ( 0, 0 ) ( 1, 3 ) ]
                                , cursor = ( 0, 0 )
                                }
                            , redoes = []
                            , version = 1
                            , changes = []
                            , pendingChanges =
                                [ Insertion ( 0, 0 ) <| B.fromString "123\n"
                                , Insertion ( 1, 0 ) <| B.fromString "123"
                                ]
                        }
                        { history | diff = [] }
            ]
        , describe "delete" <|
            let
                buf =
                    transaction
                        [ Insertion ( 0, 0 ) <| B.fromString "123"
                        , Deletion ( 0, 0 ) ( 0, 2 )
                        ]
                        emptyBuffer

                history =
                    buf.history
            in
            [ test "result.view.cursor" <|
                \_ ->
                    Expect.equal ( 0, 1 ) buf.view.cursor
            , test "result.lines" <|
                \_ ->
                    Expect.equal (B.fromString "3\n")
                        buf.lines
            , test "result.history" <|
                \_ ->
                    Expect.equal
                        { emptyBufferHistory
                            | undoes = []
                            , pending =
                                { patches =
                                    [ Insertion ( 0, 0 ) <|
                                        B.fromString "12"
                                    , Deletion ( 0, 0 ) ( 0, 3 )
                                    ]
                                , cursor = ( 0, 0 )
                                }
                            , redoes = []
                            , version = 1
                            , pendingChanges =
                                [ Insertion ( 0, 0 ) <| B.fromString "123"
                                , Deletion ( 0, 0 ) ( 0, 2 )
                                ]
                            , changes = []
                        }
                        { history | diff = [] }
            ]
        , describe "cursor" <|
            let
                buf =
                    transaction
                        [ Insertion ( 0, 0 ) <| B.fromString "\n\n"
                        , Deletion ( 0, 1 ) ( 0, 2 )
                        ]
                        emptyBuffer
            in
            [ test "result.view.cursor" <|
                \_ ->
                    Expect.equal ( 2, 0 ) buf.view.cursor
            ]
        , describe "undo insert" <|
            let
                buf =
                    emptyBuffer
                        |> insert ( 0, 0 ) "123"
                        |> commit
                        |> undo

                history =
                    buf.history
            in
            [ test "result.view.cursor" <|
                \_ ->
                    Expect.equal
                        emptyBuffer.view.cursor
                        buf.view.cursor
            , test "result.lines" <|
                \_ ->
                    Expect.equal
                        emptyBuffer.lines
                        buf.lines
            , test "result.history" <|
                \_ ->
                    Expect.equal
                        { emptyBufferHistory
                            | undoes = []
                            , pending = emptyUndo
                            , redoes =
                                [ { patches =
                                        [ Insertion ( 0, 0 ) <|
                                            B.fromString "123"
                                        ]
                                  , cursor = ( 0, 0 )
                                  }
                                ]
                            , version = 2
                        }
                        { history | diff = [] }
            ]
        , describe "redo" <|
            let
                buf =
                    emptyBuffer
                        |> insert ( 0, 0 ) "123"
                        |> commit
                        |> undo
                        |> redo

                history =
                    buf.history
            in
            [ test "cursor" <|
                \_ ->
                    Expect.equal ( 0, 0 ) buf.view.cursor
            , test "lines" <|
                \_ ->
                    Expect.equal (B.fromString "123\n") buf.lines
            , test "history" <|
                \_ ->
                    Expect.equal
                        { emptyBufferHistory
                            | undoes =
                                [ { patches = [ Deletion ( 0, 0 ) ( 0, 3 ) ]
                                  , cursor = ( 0, 0 )
                                  }
                                ]
                            , pending = emptyUndo
                            , redoes = []
                            , version = 3
                            , savePoint = 1
                            , changes =
                                [ Insertion ( 0, 0 ) <|
                                    B.fromString "123"
                                ]
                            , pendingChanges = []
                        }
                        { history | diff = [] }
            ]
        , test "undo emptyBuffer" <|
            \_ -> Expect.equal emptyBuffer (undo emptyBuffer)
        , test "redo emptyBuffer" <|
            \_ -> Expect.equal emptyBuffer (redo emptyBuffer)
        , test "undo delete" <|
            \_ ->
                let
                    buf =
                        emptyBuffer
                            |> insert ( 0, 0 ) "1234"
                            |> commit
                            |> delete ( 0, 1 ) ( 0, 2 )
                            |> commit
                            |> undo

                    history =
                        buf.history
                in
                Expect.equal
                    { emptyBufferHistory
                        | undoes =
                            [ { patches = [ Deletion ( 0, 0 ) ( 0, 4 ) ]
                              , cursor = ( 0, 0 )
                              }
                            ]
                        , pending = emptyUndo
                        , redoes =
                            [ { patches = [ Deletion ( 0, 1 ) ( 0, 2 ) ]
                              , cursor = ( 0, 4 )
                              }
                            ]
                        , savePoint = 1
                        , version = 3
                        , changes =
                            [ Insertion ( 0, 0 ) <| B.fromString "1234"
                            , Deletion ( 0, 1 ) ( 0, 2 )
                            , Insertion ( 0, 1 ) <| B.fromString "2"
                            ]
                    }
                    { history | diff = [] }
        , test "commit" <|
            \_ ->
                let
                    buf =
                        emptyBuffer
                            |> insert ( 0, 0 ) "123"
                            |> insert ( 0, 0 ) "456"
                            |> delete ( 0, 1 ) ( 0, 3 )
                            |> commit

                    history =
                        buf.history
                in
                Expect.equal
                    { emptyBufferHistory
                        | undoes =
                            [ { patches =
                                    [ Insertion ( 0, 1 ) <| B.fromString "56"
                                    , Deletion ( 0, 0 ) ( 0, 6 )
                                    ]
                              , cursor = ( 0, 0 )
                              }
                            ]
                        , pending = emptyUndo
                        , redoes = []
                        , version = 3
                        , savePoint = 1
                        , changes =
                            [ Insertion ( 0, 0 ) <| B.fromString "456123"
                            , Deletion ( 0, 1 ) ( 0, 3 )
                            ]
                    }
                    { history | diff = [] }
        , fuzz (Fuzz.list fuzzPatch) "undo random patches" <|
            \patches ->
                let
                    buf =
                        emptyBuffer
                            |> transaction patches
                            |> commit
                            |> undo
                in
                Expect.equal emptyBuffer.lines buf.lines
        , fuzz (Fuzz.list fuzzPatch) "redo random patches" <|
            \patches ->
                let
                    buf =
                        emptyBuffer
                            |> transaction patches
                            |> commit

                    buf1 =
                        buf
                            |> undo
                            |> redo
                in
                Expect.equal
                    buf.lines
                    buf1.lines
        , fuzz (Fuzz.list fuzzPatch |> Fuzz.list) "multiple undoes" <|
            \patcheslist ->
                let
                    buf =
                        List.foldl
                            (\patches buf_ ->
                                buf_
                                    |> transaction patches
                                    |> commit
                            )
                            emptyBuffer
                            patcheslist

                    buf1 =
                        repeat (List.length patcheslist) undo buf
                in
                Expect.equal emptyBuffer.lines buf1.lines
        , fuzz (Fuzz.list fuzzPatch |> Fuzz.list) "multiple redoes" <|
            \patcheslist ->
                let
                    buf =
                        List.foldl
                            (\patches buf_ ->
                                buf_
                                    |> transaction patches
                                    |> commit
                            )
                            emptyBuffer
                            patcheslist

                    buf1 =
                        List.foldl (\_ b -> undo b) buf patcheslist

                    buf2 =
                        List.foldl (\_ b -> redo b) buf1 patcheslist
                in
                Expect.equal buf.lines buf2.lines
        , describe "history items" <|
            [ fuzz (Fuzz.intRange 0 5) "undo items amount" <|
                \n ->
                    let
                        buf =
                            repeat
                                n
                                (transaction
                                    [ Insertion ( 0, 0 ) <|
                                        B.fromString "123"
                                    ]
                                    >> commit
                                )
                                emptyBuffer

                        undoes =
                            buf.history.undoes
                    in
                    Expect.equal n (List.length undoes)
            , fuzz (Fuzz.intRange 1 5) "pending patches amount" <|
                \n ->
                    let
                        buf =
                            repeat
                                n
                                (transaction
                                    [ Insertion ( 0, 0 ) <|
                                        B.fromString "123"
                                    ]
                                )
                                emptyBuffer
                    in
                    Expect.equal 1
                        (List.length buf.history.pending.patches)
            , fuzz (Fuzz.intRange 0 5) "redo items amount" <|
                \n ->
                    let
                        buf =
                            repeat
                                5
                                (transaction
                                    [ Insertion ( 0, 0 ) <|
                                        B.fromString "123"
                                    ]
                                    >> commit
                                )
                                emptyBuffer

                        buf1 =
                            repeat n undo buf

                        redoes =
                            buf1.history.redoes
                    in
                    Expect.equal n (List.length redoes)
            , test "clear redoes" <|
                \_ ->
                    let
                        insert =
                            transaction
                                [ Insertion ( 0, 0 ) <|
                                    B.fromString "123"
                                ]

                        buf =
                            emptyBuffer
                                |> insert
                                |> commit
                                |> undo
                                |> insert
                                |> commit

                        redoes =
                            buf.history.redoes
                    in
                    Expect.equal 0 (List.length redoes)
            ]
        , describe "iterateTokens"
            (let
                syntax =
                    Array.fromList
                        [ [ { length = 4
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 4
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 2
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 2
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          ]
                        ]

                lines =
                    B.fromString "func main() {}\n"
             in
             [ test "stop immediately" <|
                \_ ->
                    Expect.equal 0
                        (iterateTokens
                            True
                            (\pos line token x -> ( x, True ))
                            lines
                            syntax
                            ( 0, 0 )
                            10
                            0
                        )
             , test "count tokens" <|
                \_ ->
                    Expect.equal 7
                        (iterateTokens
                            True
                            (\pos line token x ->
                                ( x + 1, False )
                            )
                            lines
                            syntax
                            ( 0, 0 )
                            10
                            0
                        )
             , test "start at middle" <|
                \_ ->
                    Expect.equal 13
                        (iterateTokens
                            True
                            (\pos line token x ->
                                ( x + token.length, False )
                            )
                            lines
                            syntax
                            ( 0, 2 )
                            10
                            0
                        )
             , test "count start at middle" <|
                \_ ->
                    Expect.equal 6
                        (iterateTokens
                            True
                            (\pos line token x ->
                                ( x + 1, False )
                            )
                            lines
                            syntax
                            ( 0, 4 )
                            10
                            0
                        )
             , test "count backward start at middle" <|
                \_ ->
                    Expect.equal 3
                        (iterateTokens
                            False
                            (\pos line token x ->
                                ( x + 1, False )
                            )
                            lines
                            syntax
                            ( 0, 6 )
                            0
                            0
                        )
             , test "backward start at middle" <|
                \_ ->
                    Expect.equal 6
                        (iterateTokens
                            False
                            (\pos line token len ->
                                ( len + token.length, False )
                            )
                            lines
                            syntax
                            ( 0, 6 )
                            0
                            0
                        )
             ]
            )
        , describe "iterateTokens mutiple lines"
            (let
                syntax =
                    Array.fromList
                        [ [ { length = 4
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 4
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 2
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          ]
                        , [ { length = 8
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 6
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 13
                            , classname = ""
                            , tipe = TokenString
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          ]
                        , [ { length = 4
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          , { length = 1
                            , classname = ""
                            , tipe = TokenOther
                            }
                          ]
                        ]

                lines =
                    B.fromString """func main() {
        printf{"hello{world"}
    }
"""
             in
             [ test "count tokens" <|
                \_ ->
                    Expect.equal 16
                        (iterateTokens
                            True
                            (\pos line token x ->
                                ( x + 1, False )
                            )
                            lines
                            syntax
                            ( 0, 0 )
                            10
                            0
                        )
             , test "count start at middle" <|
                \_ ->
                    Expect.equal 8
                        (iterateTokens
                            True
                            (\pos line token x ->
                                ( x + 1, False )
                            )
                            lines
                            syntax
                            ( 1, 9 )
                            10
                            0
                        )
             , test "count backward start at middle" <|
                \_ ->
                    Expect.equal 8
                        (iterateTokens
                            False
                            (\pos line token x ->
                                ( x + 1, False )
                            )
                            lines
                            syntax
                            ( 1, 8 )
                            0
                            0
                        )
             , test "pair brackets in the same line success" <|
                \_ ->
                    Expect.equal (Just ( 0, 10 ))
                        (pairBracketAt 0 10 lines syntax ( 0, 9 ))
             , test "pair brackets backward in the same line success" <|
                \_ ->
                    Expect.equal (Just ( 1, 14 ))
                        (pairBracketAt 0 10 lines syntax ( 1, 28 ))
             , test "pair brackets success" <|
                \_ ->
                    Expect.equal (Just ( 2, 4 ))
                        (pairBracketAt 0 10 lines syntax ( 0, 12 ))
             , test "pair brackets backward success" <|
                \_ ->
                    Expect.equal (Just ( 0, 12 ))
                        (pairBracketAt 0 10 lines syntax ( 2, 4 ))
             ]
            )
        ]
