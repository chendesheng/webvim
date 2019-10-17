module TextBufferTest exposing (..)

import Cursor
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import TextBuffer as TB exposing (TextBuffer)


suite : Test
suite =
    describe "TextBuffer"
        [ test "insert single line" <|
            \_ ->
                Expect.equal "abc123"
                    (TB.fromString "123"
                        |> TB.insert "abc"
                        |> Tuple.first
                        |> TB.toString
                    )
        , test "delete inside line" <|
            \_ ->
                let
                    buf =
                        TB.fromString "abc123"
                in
                Expect.equal "123"
                    (buf
                        |> TB.delete 3
                        |> Tuple.first
                        |> TB.toString
                    )
        , test "undo" <|
            \_ ->
                let
                    buf =
                        TB.fromString "abc123"
                in
                Expect.equal "abc123"
                    (buf
                        |> TB.delete 3
                        |> Tuple.first
                        |> TB.commit
                        |> TB.undo
                        |> Tuple.first
                        |> TB.toString
                    )
        ]
