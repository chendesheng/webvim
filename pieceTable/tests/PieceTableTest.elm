module PieceTableTest exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import PieceTable as PT exposing (PieceTable)
import Test exposing (..)
import TextBuffer exposing (Patch(..))


emptyPieceTable =
    PT.create ""


fuzzPatch : Fuzzer Patch
fuzzPatch =
    Fuzz.oneOf
        [ Fuzz.map2 Insertion Fuzz.int Fuzz.string
        , Fuzz.map2 Deletion Fuzz.int Fuzz.int
        ]


fuzzPieceTable : Fuzzer PieceTable
fuzzPieceTable =
    Fuzz.map2
        (List.foldl applyPatch)
        (Fuzz.map PT.create Fuzz.string)
        (Fuzz.list fuzzPatch)


applyPatch : Patch -> PieceTable -> PieceTable
applyPatch patch t =
    case patch of
        Insertion start s ->
            insert start s t

        Deletion from to ->
            delete from to t


insert : Int -> String -> PieceTable -> PieceTable
insert start s t =
    case PT.cursorAddOffsets start t PT.zeroCursor of
        Just cursor ->
            PT.insert cursor s t

        _ ->
            t


delete : Int -> Int -> PieceTable -> PieceTable
delete from to t =
    let
        cur0 =
            PT.zeroCursor

        curFrom =
            PT.cursorAddOffsets from t cur0
                |> Maybe.withDefault cur0

        curTo =
            PT.cursorAddOffsets (to - from) t curFrom
                |> Maybe.withDefault (PT.maxCursor t)
    in
    t
        |> PT.delete curFrom curTo


checkLines : List String -> PieceTable -> List (a -> Expectation)
checkLines lines t =
    lines
        |> List.foldl
            (\line ( cursor, tests ) ->
                ( cursor
                    |> PT.cursorAddLines 1 t
                    |> Maybe.withDefault cursor
                , (\_ -> Expect.equal line (PT.getLine t cursor))
                    :: tests
                )
            )
            ( PT.zeroCursor, [] )
        |> Tuple.second
        |> List.reverse


suite : Test
suite =
    describe "Piece Table"
        [ test "Empty piece table" <|
            Expect.all
                [ \_ -> Expect.equal 1 <| PT.totalLines emptyPieceTable
                , \_ -> Expect.equal 0 <| PT.totalLength emptyPieceTable
                , \_ -> Expect.equal "" <| PT.toString emptyPieceTable
                ]
        , describe "Inserts"
            [ test "insert single line" <|
                let
                    t =
                        emptyPieceTable
                            |> PT.insert PT.zeroCursor "abc"
                            --|> Debug.log "Inserts abc"
                            |> PT.insert PT.zeroCursor "123"

                    --|> Debug.log "Inserts 123"
                in
                Expect.all
                    [ \_ -> Expect.equal 1 <| PT.totalLines t
                    , \_ -> Expect.equal 6 <| PT.totalLength t
                    , \_ -> Expect.equal "123abc" <| PT.toString t
                    ]
            , test "insert at piece end and merge" <|
                let
                    t =
                        emptyPieceTable
                            |> insert 0 "abc"
                            |> insert 3 "123"
                in
                Expect.all
                    [ \_ -> Expect.equal 2 (Array.length t.pieces)
                    , \_ -> Expect.equal "abc123" (PT.toString t)
                    ]
            , test "insert at piece start and merge" <|
                let
                    t =
                        emptyPieceTable
                            |> insert 0 "abc"
                            |> insert 0 "123"
                            |> insert 3 "4"

                    --|> Debug.log "insert at piece start and merge"
                in
                Expect.all
                    [ \_ -> Expect.equal "1234abc" (PT.toString t)
                    , \_ -> Expect.equal 3 (Array.length t.pieces)
                    ]
            , test "insert multiple lines" <|
                let
                    t =
                        emptyPieceTable
                            |> insert 0 "abc"
                            |> insert 3 "\n"
                            |> insert 0 "1\u{000D}\n23\n"

                    --|> Debug.log "test case"
                in
                Expect.all <|
                    [ \_ -> Expect.equal "1\u{000D}\n23\nabc\n" <| PT.toString t
                    , \_ ->
                        Expect.equal "abc\n" <|
                            PT.getLine
                                t
                                (PT.cursorAddLines 2 t PT.zeroCursor
                                    |> Maybe.withDefault PT.zeroCursor
                                 --|> Debug.log "cursor"
                                )
                    , \_ -> Expect.equal 4 <| PT.totalLines t
                    , \_ -> Expect.equal 10 <| PT.totalLength t
                    ]
                        ++ checkLines [ "1\u{000D}\n", "23\n", "abc\n", "" ] t
            ]
        , describe "Deletes"
            [ test "delete inside line" <|
                let
                    t =
                        PT.create "0123456789"
                            |> delete 0 2
                            |> delete 7 8
                in
                Expect.all
                    [ \_ -> Expect.equal 7 <| PT.totalLength t
                    , \_ -> Expect.equal "2345678" (PT.getLine t PT.zeroCursor)
                    ]
            , test "delete cross lines" <|
                let
                    t =
                        PT.create "01\nab\u{000D}\n\u{000D}\ncde\nf\n"
                            |> delete 0 2
                            |> delete 2 6

                    --|> Debug.log "delete table"
                in
                Expect.all <|
                    [ \_ -> Expect.equal 9 <| PT.totalLength t
                    , \_ -> Expect.equal "\na\ncde\nf\n" <| PT.toString t
                    , \_ -> Expect.equal 5 <| PT.totalLines t
                    ]
                        ++ checkLines [ "\n", "a\n", "cde\n", "f\n", "" ] t
            ]
        , let
            t =
                emptyPieceTable
                    |> PT.insert PT.zeroCursor "aaa\n"
                    |> PT.insert PT.zeroCursor "bbb\n"
                    |> PT.insert PT.zeroCursor "ccc"
                    |> PT.insert PT.zeroCursor "ddd"

            --|> Debug.log "move 1 line forward"
          in
          describe "Move by line"
            [ test "move 1 line forward" <|
                let
                    maybePos =
                        PT.cursorAddLines 1 t PT.zeroCursor
                in
                Expect.all
                    [ \_ ->
                        Expect.equal (Just ( 2, 0 )) <| Maybe.map (PT.getPoint t) maybePos
                    ]
            , test "move 1 line backward" <|
                \_ ->
                    Expect.equal
                        (Just
                            PT.zeroCursor
                        )
                        (PT.zeroCursor
                            |> PT.cursorAddLines 1 t
                            |> Maybe.andThen (PT.cursorAddLines -1 t)
                        )
            , test "move 2 lines forward" <|
                \_ ->
                    Expect.equal
                        (Just ( 3, 0 ))
                        (PT.zeroCursor
                            |> PT.cursorAddLines 2 t
                            |> Maybe.map (PT.getPoint t)
                        )
            , test "move 2 lines backward" <|
                \_ ->
                    Expect.equal
                        (Just PT.zeroCursor)
                        (PT.zeroCursor
                            |> PT.cursorAddLines 2 t
                            |> Maybe.andThen (PT.cursorAddLines -2 t)
                        )
            ]
        , let
            t =
                emptyPieceTable
                    |> PT.insert PT.zeroCursor "1\n1\n1\n"
                    |> PT.insert PT.zeroCursor "aaa\n"
                    |> PT.insert PT.zeroCursor "bbb\n"
                    |> PT.insert PT.zeroCursor "ccc"
                    |> PT.insert PT.zeroCursor "ddd"
          in
          describe "move by char"
            [ test "move char forward cross line" <|
                Expect.all <|
                    checkLines [ "dddcccbbb\n", "aaa\n", "1\n", "1\n", "1\n" ] t
                        ++ [ \_ ->
                                Expect.equal
                                    (Just ( 2, 1 ))
                                    (PT.cursorAddOffsets 11 t PT.zeroCursor
                                        |> Maybe.map (PT.getPoint t)
                                    )
                           ]
            , test "move 1 char forward" <|
                \_ ->
                    Expect.equal
                        (Just ( 1, 1 ))
                        (PT.cursorAddOffsets 1 t PT.zeroCursor
                            |> Maybe.map (PT.getPoint t)
                        )
            , test "move char forward cross piece" <|
                \_ ->
                    Expect.equal
                        (Just ( 1, 4 ))
                        (PT.cursorAddOffsets 4 t PT.zeroCursor
                            |> Maybe.map (PT.getPoint t)
                        )
            , test "move char forward multiple line in one piece" <|
                \_ ->
                    let
                        t1 =
                            PT.insert PT.zeroCursor "1\n1\n1\n" emptyPieceTable

                        --|> Debug.log "t1"
                    in
                    Expect.equal
                        (Just ( 3, 0 ))
                        (PT.cursorAddOffsets 4 t1 PT.zeroCursor
                            |> Maybe.map (PT.getPoint t)
                        )
            , test "just \\n" <|
                \_ ->
                    let
                        t1 =
                            emptyPieceTable
                                |> PT.insert PT.zeroCursor "\n"

                        --|> Debug.log "fuzz test sample"
                        pos =
                            PT.zeroCursor
                                |> PT.cursorAddOffsets 1 t1
                                --|> Debug.log "fuzz test sample1"
                                |> Maybe.andThen (PT.cursorAddOffsets -1 t1)
                    in
                    Expect.equal (Just 1) (Maybe.map PT.getRow pos)
            ]
        , let
            t =
                emptyPieceTable
                    |> PT.insert PT.zeroCursor "1\n\n"
                    |> PT.insert PT.zeroCursor "aa\n"
                    |> PT.insert PT.zeroCursor "bb\n"
                    |> PT.insert PT.zeroCursor "cc"
                    |> PT.insert PT.zeroCursor "dd"
          in
          describe "iterate"
            [ test "forward all lines" <|
                \_ ->
                    Expect.equal [ "ddccbb\n", "aa\n", "1\n", "\n", "" ]
                        (PT.iterateLinesForward
                            PT.zeroCursor
                            t
                            (\line cur1 cur2 res ->
                                Just <| line :: res
                            )
                            []
                            |> List.reverse
                        )
            , test "backward all lines" <|
                \_ ->
                    Expect.equal [ "ddccbb\n", "aa\n", "1\n", "\n", "" ]
                        (PT.iterateLinesBackward
                            (PT.maxCursor t)
                            t
                            (\line cur1 cur2 res ->
                                Just <| line :: res
                            )
                            []
                        )
            ]
        , fuzz fuzzPieceTable "iteration result should be the same in diffferent directions" <|
            \t ->
                Expect.equal
                    (PT.iterateLinesForward
                        PT.zeroCursor
                        t
                        (\line cur1 cur2 res ->
                            Just <| line :: res
                        )
                        []
                        |> List.reverse
                    )
                    (PT.iterateLinesBackward
                        (PT.maxCursor t)
                        t
                        (\line cur1 cur2 res ->
                            Just <| line :: res
                        )
                        []
                    )
        , fuzz2 fuzzPieceTable Fuzz.int "move by line back and forth" <|
            \t n ->
                case PT.cursorAddLines n t PT.zeroCursor of
                    Just cursor ->
                        Expect.equal (Just PT.zeroCursor) <|
                            PT.cursorAddLines -n t cursor

                    _ ->
                        Expect.false "should out side of lines"
                            (0 <= n && n < PT.totalLines t)
        , fuzz2 fuzzPieceTable Fuzz.int "move by char back and forth" <|
            \t n ->
                case PT.cursorAddOffsets n t PT.zeroCursor of
                    Just cursor ->
                        --let
                        --    _ =
                        --        Debug.log "move by char back and forth" ( t, cursor, n )
                        --in
                        Expect.equal (Just 1) <|
                            (Maybe.map PT.getRow <| PT.cursorAddOffsets -n t cursor)

                    _ ->
                        Expect.false "should out side of lines"
                            (0 <= n && n < PT.totalLength t)
        ]



--[ test "delete cross lines" <|
--    let
--        t =
--            PT.create "01\nab\u{000D}\n\u{000D}\ncde\nf\n"
--                |> delete 0 2
--                |> delete 2 6
--                |> Debug.log "delete table"
--    in
--    Expect.all <|
--        checkLines [ "\n", "a\n", "cde\n", "f\n" ] t
--]
