module TextBuffer exposing (..)

import Fuzz
import Expect exposing (Expectation)
import Test exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer)
import Types exposing (..)
import Array


normalPatches :
    List
        { input : ( Patch, TextBuffer )
        , label : String
        , output : ( Patch, TextBuffer )
        }
normalPatches =
    [ { label = "insert `123` to empty string"
      , input =
            ( Insertion ( 0, 0 ) "123"
            , B.empty
            )
      , output =
            ( Deletion ( 0, 0 ) ( 0, 3 )
            , B.fromString "123"
            )
      }
    , { label = "insert `123\\n` to empty string"
      , input =
            ( Insertion ( 0, 0 ) "123\n"
            , B.empty
            )
      , output =
            ( Deletion ( 0, 0 ) ( 1, 0 )
            , B.fromString "123\n"
            )
      }
    , { label = "insert `123\\n123` to empty string"
      , input =
            ( Insertion ( 0, 0 ) "123\n123"
            , B.empty
            )
      , output =
            ( Deletion ( 0, 0 ) ( 1, 3 )
            , B.fromString "123\n123"
            )
      }
    , { label = "insert `123\\n123\\n` to empty string"
      , input =
            ( Insertion ( 0, 0 ) "123\n123\n"
            , B.empty
            )
      , output =
            ( Deletion ( 0, 0 ) ( 2, 0 )
            , B.fromString "123\n123\n"
            )
      }
    , { label = "insert empty string"
      , input =
            ( Insertion ( 1, 0 ) ""
            , B.fromString "123\n123"
            )
      , output =
            ( Deletion ( 1, 0 ) ( 1, 0 )
            , B.fromString "123\n123"
            )
      }
    , { label = "insert `123` into middle of buffer"
      , input =
            ( Insertion ( 1, 0 ) "123"
            , B.fromString "abc\ndef"
            )
      , output =
            ( Deletion ( 1, 0 ) ( 1, 3 )
            , B.fromString "abc\n123def"
            )
      }
    , { label = "insert `123\\n456` into middle of buffer"
      , input =
            ( Insertion ( 1, 1 ) "123\n456"
            , B.fromString "abc\ndef"
            )
      , output =
            ( Deletion ( 1, 1 ) ( 2, 3 )
            , B.fromString "abc\nd123\n456ef"
            )
      }
    , { label = "insert `123\\n456\\n` into middle of buffer"
      , input =
            ( Insertion ( 2, 0 ) "123\n456\n"
            , B.fromString "abc\ndef\n\n"
            )
      , output =
            ( Deletion ( 2, 0 ) ( 4, 0 )
            , B.fromString "abc\ndef\n123\n456\n\n"
            )
      }
    , { label = "append `123\\n456` to a buffer"
      , input =
            ( Insertion ( 2, 0 ) "123\n456"
            , B.fromString "abc\ndef\n"
            )
      , output =
            ( Deletion ( 2, 0 ) ( 3, 3 )
            , B.fromString "abc\ndef\n123\n456"
            )
      }
    ]


randomBuffer : Fuzz.Fuzzer TextBuffer
randomBuffer =
    Fuzz.map B.fromString Fuzz.string


randomPosition : Fuzz.Fuzzer Position
randomPosition =
    Fuzz.tuple ( Fuzz.int, Fuzz.int )


randomPatch : Fuzz.Fuzzer Patch
randomPatch =
    Fuzz.oneOf
        [ Fuzz.map2 Insertion randomPosition Fuzz.string
        , Fuzz.map2
            Deletion
            randomPosition
            randomPosition
        ]


suite : Test
suite =
    describe "interfaces" <|
        [ describe "fromString"
            [ test "from `123`" <|
                \_ ->
                    Expect.equal
                        (B.fromList [ "123" ])
                        (B.fromString "123")
            , test "from `123\\n`" <|
                \_ ->
                    Expect.equal
                        (B.fromList [ "123\n", "" ])
                        (B.fromString "123\n")
            , test "from `123\\n123`" <|
                \_ ->
                    Expect.equal
                        (B.fromList [ "123\n", "123" ])
                        (B.fromString "123\n123")
            ]
        , describe "slice"
            [ test "slice (0,0) (0,3) `123`" <|
                \_ ->
                    Expect.equal
                        (B.fromString "123")
                        ("123"
                            |> B.fromString
                            |> B.slice ( 0, 0 ) ( 0, 3 )
                        )
            , test "slice (0,0) (1,0) `123\\n`" <|
                \_ ->
                    Expect.equal
                        (B.fromString "123\n")
                        ("123\n"
                            |> B.fromString
                            |> B.slice ( 0, 0 ) ( 1, 0 )
                        )
            , test "slice (0,0) (2,0) `123\\n123\\n`" <|
                \_ ->
                    Expect.equal
                        (B.fromString "123\n123\n")
                        ("123\n123\n"
                            |> B.fromString
                            |> B.slice ( 0, 0 ) ( 2, 0 )
                        )
            ]
        , describe "applyPatch"
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
        , describe "by property"
            [ fuzz
                (Fuzz.tuple ( randomPatch, randomBuffer ))
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
                (Fuzz.tuple ( randomPatch, randomBuffer ))
                "applyPatch always return valid buffer"
              <|
                \( patch, buf ) ->
                    Expect.all
                        [ (\buf ->
                            let
                                line =
                                    B.countLines buf
                                        |> flip B.getLine buf
                                        |> Maybe.withDefault ""
                            in
                                String.endsWith B.lineBreak line
                                    |> Expect.false
                                        "last line should not endsWith \\n"
                          )
                        , (\buf ->
                            B.mapLines String.isEmpty buf
                                |> Array.slice 0
                                    (B.countLines buf - 1)
                                |> Array.toList
                                |> List.all not
                                |> Expect.true
                                    "should not contains empty line except last line"
                          )
                        ]
                        (B.applyPatch patch buf |> Tuple.second)
            ]
        ]
