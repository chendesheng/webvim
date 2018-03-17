module TextBuffer exposing (..)

import Fuzz
import Expect exposing (Expectation)
import Test exposing (..)
import Internal.TextBuffer as B exposing (TextBuffer, Patch(..))
import Position exposing (..)
import Array
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
            ( Insertion ( 0, 0 ) <| B.fromList [ "123" ]
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
            ( Insertion ( 2, 1 ) B.empty
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


fuzzPatch : Fuzz.Fuzzer Patch
fuzzPatch =
    Fuzz.oneOf
        [ Fuzz.map B.fromString Fuzz.string
            |> Fuzz.map2 Insertion fuzzPosition
        , fuzzPosition
            |> Fuzz.andThen
                (\pos ->
                    Fuzz.map (Deletion pos) (fuzzPositionFrom pos)
                )
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
                                        |> B.getLine (B.countLines buf - 1)
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
                                    (B.countLines buf - 1)
                                |> Array.toList
                                |> List.all ((==) True)
                                |> Expect.true
                                    "should always contains single \\n except last line"
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
