module BufferTest exposing (..)

import Fuzz
import Expect exposing (Expectation)
import Test exposing (..)
import Buffer exposing (..)
import Model exposing (Buffer, emptyBuffer, BufferHistory, emptyBufferHistory)
import Internal.TextBuffer as B exposing (Patch(..))
import TextBuffer exposing (..)


repeat : Int -> (a -> a) -> (a -> a)
repeat n f =
    let
        helper n x =
            case (max n 0) of
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
            in
                [ test "result.cursor" <|
                    \_ ->
                        Expect.equal ( 0, 3 ) buf.cursor
                , test "result.lines" <|
                    \_ ->
                        Expect.equal (B.fromString "123\n")
                            buf.lines
                , test "result.history" <|
                    \_ ->
                        Expect.equal
                            { emptyBufferHistory
                                | undoes = []
                                , pending = [ Deletion ( 0, 0 ) ( 0, 3 ) ]
                                , redoes = []
                                , version = 1
                            }
                            buf.history
                ]
        , describe "insert patches" <|
            let
                buf =
                    transaction
                        [ Insertion ( 0, 0 ) <| B.fromString "123\n"
                        , Insertion ( 1, 0 ) <| B.fromString "123"
                        ]
                        emptyBuffer
            in
                [ test "result.cursor" <|
                    \_ ->
                        Expect.equal ( 1, 3 ) buf.cursor
                , test "result.lines" <|
                    \_ ->
                        Expect.equal (B.fromString "123\n123\n")
                            buf.lines
                , test "result.history" <|
                    \_ ->
                        Expect.equal
                            { emptyBufferHistory
                                | undoes =
                                    []
                                , pending =
                                    [ Deletion ( 0, 0 ) ( 1, 3 ) ]
                                , redoes = []
                                , version = 1
                            }
                            buf.history
                ]
        , describe "delete" <|
            let
                buf =
                    transaction
                        [ Insertion ( 0, 0 ) <| B.fromString "123"
                        , Deletion ( 0, 0 ) ( 0, 2 )
                        ]
                        emptyBuffer
            in
                [ test "result.cursor" <|
                    \_ ->
                        Expect.equal ( 0, 1 ) buf.cursor
                , test "result.lines" <|
                    \_ ->
                        Expect.equal (B.fromString "3\n")
                            buf.lines
                , test "result.history" <|
                    \_ ->
                        Expect.equal
                            { emptyBufferHistory
                                | undoes =
                                    []
                                , pending =
                                    [ Insertion ( 0, 0 ) <|
                                        B.fromString "12"
                                    , Deletion ( 0, 0 ) ( 0, 3 )
                                    ]
                                , redoes = []
                                , version = 1
                            }
                            buf.history
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
                [ test "result.cursor" <|
                    \_ ->
                        Expect.equal ( 2, 0 ) buf.cursor
                ]
        , describe "undo insert" <|
            let
                buf =
                    emptyBuffer
                        |> insert ( 0, 0 ) "123"
                        |> commit
                        |> undo
            in
                [ test "result.cursor" <|
                    \_ ->
                        Expect.equal
                            emptyBuffer.cursor
                            buf.cursor
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
                                , pending = []
                                , redoes =
                                    [ [ Insertion ( 0, 0 ) <|
                                            B.fromString "123"
                                      ]
                                    ]
                                , version = 2
                            }
                            buf.history
                ]
        , describe "redo" <|
            let
                buf =
                    emptyBuffer
                        |> insert ( 0, 0 ) "123"
                        |> commit
                        |> undo
                        |> redo
            in
                [ test "cursor" <|
                    \_ ->
                        Expect.equal ( 0, 0 ) buf.cursor
                , test "lines" <|
                    \_ ->
                        Expect.equal (B.fromString "123\n") buf.lines
                , test "history" <|
                    \_ ->
                        Expect.equal
                            { emptyBufferHistory
                                | undoes =
                                    [ [ Deletion ( 0, 0 ) ( 0, 3 ) ] ]
                                , pending = []
                                , redoes = []
                                , version = 3
                            }
                            buf.history
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
                in
                    Expect.equal
                        { emptyBufferHistory
                            | undoes =
                                [ [ Deletion ( 0, 0 ) ( 0, 4 ) ] ]
                            , pending = []
                            , redoes =
                                [ [ Deletion ( 0, 1 ) ( 0, 2 ) ] ]
                            , version = 3
                        }
                        buf.history
        , test "commit" <|
            \_ ->
                let
                    buf =
                        emptyBuffer
                            |> insert ( 0, 0 ) "123"
                            |> insert ( 0, 0 ) "456"
                            |> delete ( 0, 1 ) ( 0, 3 )
                            |> commit
                in
                    Expect.equal
                        { emptyBufferHistory
                            | undoes =
                                [ [ Insertion ( 0, 1 ) <| B.fromString "56"
                                  , Deletion ( 0, 0 ) ( 0, 3 )
                                  , Deletion ( 0, 0 ) ( 0, 3 )
                                  ]
                                ]
                            , pending = []
                            , redoes = []
                            , version = 3
                        }
                        buf.history
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
        , fuzz
            (Fuzz.list fuzzPatch |> Fuzz.list)
            "multiple undoes"
          <|
            \patcheslist ->
                let
                    buf =
                        List.foldl
                            (\patches buf ->
                                buf
                                    |> transaction patches
                                    |> commit
                            )
                            emptyBuffer
                            patcheslist

                    buf1 =
                        List.foldl (\_ b -> undo b) buf patcheslist
                in
                    Expect.equal
                        emptyBuffer.lines
                        buf1.lines
        , fuzz
            (Fuzz.list fuzzPatch |> Fuzz.list)
            "multiple redoes"
          <|
            \patcheslist ->
                let
                    buf =
                        List.foldl
                            (\patches buf ->
                                buf
                                    |> transaction patches
                                    |> commit
                            )
                            emptyBuffer
                            patcheslist

                    buf1 =
                        List.foldl (\_ b -> undo b) buf patcheslist

                    buf2 =
                        List.foldl (\_ b -> undo b) buf1 patcheslist
                in
                    Expect.equal buf1.lines buf2.lines
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
            , fuzz (Fuzz.intRange 0 5) "pending patches amount" <|
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
                        Expect.equal n
                            (List.length buf.history.pending)
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
        ]
