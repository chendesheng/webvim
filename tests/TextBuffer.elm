module TextBuffer exposing (..)

import Fuzz
import Expect exposing (Expectation)
import Test exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer, Patch(..))
import Internal.Position exposing (..)
import Elm.Array as Array
import String


normalPatches :
    List
        { input : ( Patch, TextBuffer )
        , label : String
        , output : ( Patch, TextBuffer )
        }
normalPatches =
    [ { label = "insert `123` to empty string"
      , input =
            ( Insertion ( 0, 0 ) <| B.fromString "123"
            , B.empty
            )
      , output =
            ( Deletion ( 0, 0 ) ( 0, 3 )
            , B.fromString "123"
            )
      }
    , { label = "insert `123\\n` to empty string"
      , input =
            ( Insertion ( 0, 0 ) <| B.fromString "123\n"
            , B.empty
            )
      , output =
            ( Deletion ( 0, 0 ) ( 1, 0 )
            , B.fromString "123\n"
            )
      }
    , { label = "insert `123\\n123` to empty string"
      , input =
            ( Insertion ( 0, 0 ) <| B.fromString "123\n123"
            , B.empty
            )
      , output =
            ( Deletion ( 0, 0 ) ( 1, 3 )
            , B.fromString "123\n123"
            )
      }
    , { label = "insert `123\\n123\\n` to empty string"
      , input =
            ( Insertion ( 0, 0 ) <| B.fromString "123\n123\n"
            , B.empty
            )
      , output =
            ( Deletion ( 0, 0 ) ( 2, 0 )
            , B.fromString "123\n123\n"
            )
      }
    , { label = "insert empty string"
      , input =
            ( Insertion ( 1, 0 ) B.empty
            , B.fromString "123\n123"
            )
      , output =
            ( Deletion ( 1, 0 ) ( 1, 0 )
            , B.fromString "123\n123"
            )
      }
    , { label = "insert `123` into middle of buffer"
      , input =
            ( Insertion ( 1, 0 ) <| B.fromString "123"
            , B.fromString "abc\ndef"
            )
      , output =
            ( Deletion ( 1, 0 ) ( 1, 3 )
            , B.fromString "abc\n123def"
            )
      }
    , { label = "insert `123\\n456` into middle of buffer"
      , input =
            ( Insertion ( 1, 1 ) <| B.fromString "123\n456"
            , B.fromString "abc\ndef"
            )
      , output =
            ( Deletion ( 1, 1 ) ( 2, 3 )
            , B.fromString "abc\nd123\n456ef"
            )
      }
    , { label = "insert `123\\n456\\n` into middle of buffer"
      , input =
            ( Insertion ( 2, 0 ) <| B.fromString "123\n456\n"
            , B.fromString "abc\ndef\n\n"
            )
      , output =
            ( Deletion ( 2, 0 ) ( 4, 0 )
            , B.fromString "abc\ndef\n123\n456\n\n"
            )
      }
    , { label = "append `123\\n456` to a buffer"
      , input =
            ( Insertion ( 2, 0 ) <| B.fromString "123\n456"
            , B.fromString "abc\ndef\n"
            )
      , output =
            ( Deletion ( 2, 0 ) ( 3, 3 )
            , B.fromString "abc\ndef\n123\n456"
            )
      }
    ]


invalidPatches :
    List
        { input : ( Patch, TextBuffer )
        , label : String
        , output : ( Patch, TextBuffer )
        }
invalidPatches =
    [ { label = "delete exceed line end"
      , input =
            ( Deletion ( 2, 1 ) ( 2, 2 )
            , B.fromString "\n\n\n\n"
            )
      , output =
            ( Insertion ( 3, 0 ) B.empty
            , B.fromString "\n\n\n\n"
            )
      }
    ]


fuzzBuffer : Fuzz.Fuzzer TextBuffer
fuzzBuffer =
    Fuzz.map B.fromString Fuzz.string


fuzzPosition : Fuzz.Fuzzer Position
fuzzPosition =
    Fuzz.tuple ( Fuzz.intRange 0 1000, Fuzz.intRange 0 1000 )


fuzzPositionFrom : Position -> Fuzz.Fuzzer Position
fuzzPositionFrom ( y, x ) =
    Fuzz.tuple ( Fuzz.intRange y (y + 1000), Fuzz.intRange x (x + 1000) )


fuzzLines : Fuzz.Fuzzer B.TextBuffer
fuzzLines =
    Fuzz.string
        |> Fuzz.andThen
            (\s ->
                Fuzz.intRange 0 (String.length s)
                    |> Fuzz.list
                    |> Fuzz.map2
                        (\n poses ->
                            poses
                                |> List.take n
                                |> List.sort
                                |> List.reverse
                                |> List.foldl
                                    (\i res ->
                                        String.left i res
                                            ++ "\n"
                                            ++ String.dropLeft i res
                                    )
                                    s
                                |> B.fromString
                        )
                        (Fuzz.intRange 0 10)
            )


fuzzPatch : Fuzz.Fuzzer Patch
fuzzPatch =
    Fuzz.oneOf
        [ Fuzz.map2 Insertion fuzzPosition fuzzLines
        , fuzzPosition
            |> Fuzz.andThen
                (\pos ->
                    Fuzz.map (Deletion pos) (fuzzPositionFrom pos)
                )
        ]


suite : Test
suite =
    describe "interfaces" <|
        [ describe "applyPatch"
            (List.concatMap
                (\{ label, input, output } ->
                    let
                        ( patch, buf ) =
                            input

                        ( patch1, buf1 ) =
                            output
                    in
                        [ test label <|
                            \_ ->
                                Expect.equal
                                    ( patch1, buf1 )
                                    (B.applyPatch patch buf)
                        , test ("Revert: " ++ label) <|
                            \_ ->
                                Expect.equal
                                    ( patch, buf )
                                    (B.applyPatch patch1 buf1)
                        ]
                )
                normalPatches
            )
        , describe "apply invalid patch"
            (List.concatMap
                (\{ label, input, output } ->
                    let
                        ( patch, buf ) =
                            input

                        ( patch1, buf1 ) =
                            output
                    in
                        [ test label <|
                            \_ ->
                                Expect.equal
                                    ( patch1, buf1 )
                                    (B.applyPatch patch buf)
                        ]
                )
                invalidPatches
            )
        , describe "by property"
            [ fuzz
                (Fuzz.tuple ( fuzzPatch, fuzzBuffer ))
                "apply a patch returned from applyPatch get orignal buffer"
              <|
                \( patch, buf ) ->
                    let
                        ( patch1, buf1 ) =
                            B.applyPatch patch buf

                        ( patch2, buf2 ) =
                            B.applyPatch patch1 buf1
                    in
                        Expect.equal buf buf2
            , fuzz
                (Fuzz.tuple ( fuzzPatch, fuzzBuffer ))
                "applyPatch always return valid buffer"
              <|
                \( patch, buf ) ->
                    Expect.all
                        [ (\buf ->
                            let
                                line =
                                    buf
                                        |> B.getLine (B.count buf - 1)
                                        |> Maybe.withDefault ""
                            in
                                String.endsWith B.lineBreak line
                                    |> Expect.false
                                        "last line should not endsWith \\n"
                          )
                        , (\buf ->
                            B.mapLines
                                (\line ->
                                    line
                                        |> String.indexes B.lineBreak
                                        |> List.length
                                        |> ((==) 1)
                                )
                                buf
                                |> Array.slice 0
                                    (B.count buf - 1)
                                |> Array.toList
                                |> List.all ((==) True)
                                |> Expect.true
                                    "should always contains single \\n except last line"
                          )
                        , (\buf ->
                            B.mapLines String.isEmpty buf
                                |> Array.slice 0
                                    (B.count buf - 1)
                                |> Array.toList
                                |> List.all not
                                |> Expect.true
                                    "should not contains empty line except last line"
                          )
                        ]
                        (B.applyPatch patch buf |> Tuple.second)
            ]
        , let
            testMergePatch cases =
                let
                    ( a, b ) =
                        cases.patches
                in
                    test cases.label <|
                        \_ ->
                            Expect.equal
                                (B.mergePatch a b)
                                cases.result
          in
            describe "mergePatch"
                [ describe "insert + insert"
                    (List.map testMergePatch
                        [ { label = "merge success"
                          , patches =
                                ( Insertion ( 0, 1 ) (B.fromString "2")
                                , Insertion ( 0, 0 ) (B.fromString "1")
                                )
                          , result = Just <| Insertion ( 0, 0 ) (B.fromString "12")
                          }
                        , { label = "merge mutiple lines success"
                          , patches =
                                ( Insertion ( 1, 1 ) (B.fromString "2")
                                , Insertion ( 0, 0 ) (B.fromString "13344\n2")
                                )
                          , result =
                                Just <|
                                    Insertion ( 0, 0 )
                                        (B.fromString "13344\n22")
                          }
                        , { label = "merge failed"
                          , patches =
                                ( Insertion ( 0, 0 ) (B.fromString "1")
                                , Insertion ( 0, 2 ) (B.fromString "2")
                                )
                          , result = Nothing
                          }
                        ]
                    )
                , describe "delete + delete"
                    (List.map testMergePatch
                        [ { label = "merge success"
                          , patches =
                                ( Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                )
                          , result = Just <| Deletion ( 0, 0 ) ( 0, 2 )
                          }
                        , { label = "merge failed"
                          , patches =
                                ( Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 1 ) ( 0, 2 )
                                )
                          , result = Nothing
                          }
                        ]
                    )
                ]
        , describe "shiftPositionByPatch"
            [ describe "insertion"
                [ test "before" <|
                    \_ ->
                        Expect.equal
                            ( 61, 13 )
                            (B.shiftPositionByPatch
                                (Insertion ( 57, 0 ) (B.fromString "\n"))
                                ( 60, 13 )
                            )
                , test "before, single line" <|
                    \_ ->
                        Expect.equal
                            ( 60, 13 )
                            (B.shiftPositionByPatch
                                (Insertion ( 57, 0 ) (B.fromString "123"))
                                ( 60, 13 )
                            )
                , test "after" <|
                    \_ ->
                        Expect.equal
                            ( 60, 13 )
                            (B.shiftPositionByPatch
                                (Insertion ( 67, 0 ) (B.fromString "\n"))
                                ( 60, 13 )
                            )
                , test "same line" <|
                    \_ ->
                        Expect.equal
                            ( 60, 16 )
                            (B.shiftPositionByPatch
                                (Insertion ( 60, 13 ) (B.fromString "123"))
                                ( 60, 13 )
                            )
                , test "same line, insert mutiple lines" <|
                    \_ ->
                        Expect.equal
                            ( 62, 13 )
                            (B.shiftPositionByPatch
                                (Insertion ( 61, 0 ) (B.fromString "    \n"))
                                ( 61, 13 )
                            )
                ]
            , describe "deletion"
                [ test "before" <|
                    \_ ->
                        Expect.equal
                            ( 61, 13 )
                            (B.shiftPositionByPatch
                                (Deletion ( 60, 13 ) ( 60, 15 ))
                                ( 61, 13 )
                            )
                , test "delete whole line" <|
                    \_ ->
                        Expect.equal
                            ( 60, 13 )
                            (B.shiftPositionByPatch
                                (Deletion ( 60, 0 ) ( 61, 0 ))
                                ( 61, 13 )
                            )
                , test "same line before" <|
                    \_ ->
                        Expect.equal
                            ( 60, 13 )
                            (B.shiftPositionByPatch
                                (Deletion ( 60, 13 ) ( 60, 15 ))
                                ( 60, 13 )
                            )
                , test "after" <|
                    \_ ->
                        Expect.equal
                            ( 61, 13 )
                            (B.shiftPositionByPatch
                                (Deletion ( 60, 13 ) ( 61, 12 ))
                                ( 62, 13 )
                            )
                , test "multipe lines deletion, same line after" <|
                    \_ ->
                        Expect.equal
                            ( 60, 1 )
                            (B.shiftPositionByPatch
                                (Deletion ( 60, 13 ) ( 61, 12 ))
                                ( 61, 13 )
                            )
                , test "same line after" <|
                    \_ ->
                        Expect.equal
                            ( 60, 14 )
                            (B.shiftPositionByPatch
                                (Deletion ( 60, 13 ) ( 60, 14 ))
                                ( 60, 15 )
                            )
                , test "contains" <|
                    \_ ->
                        Expect.equal
                            ( 60, 13 )
                            (B.shiftPositionByPatch
                                (Deletion ( 60, 13 ) ( 60, 18 ))
                                ( 60, 15 )
                            )
                ]
            ]
        ]
