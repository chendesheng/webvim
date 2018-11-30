module TestViewLines exposing (..)

import Fuzz
import Expect exposing (Expectation)
import Test exposing (..)
import Update.Buffer exposing (..)
import TextBuffer exposing (..)
import Model exposing (Buffer, emptyBuffer)
import Internal.TextBuffer exposing (Patch(..), fromString)


resize : Int -> Buffer -> Buffer
resize height buf =
    let
        view =
            buf.view
    in
        { buf
            | view =
                { view
                    | size = { height = height, width = 1 }
                    , lines = List.range 0 (height + 1)
                }
        }


applyPatches : Int -> List Patch -> Expectation
applyPatches height patches =
    let
        buf =
            emptyBuffer
                |> resize height
                |> transaction patches
    in
        Expect.equal
            (List.range buf.view.scrollTop (buf.view.scrollTop + buf.view.size.height + 1))
            (List.sort buf.view.lines)


suite : Test
suite =
    describe "view lines"
        [ fuzz2
            (Fuzz.intRange 1 5)
            (Fuzz.list fuzzPatch)
            "sorted viewLines should always equal to lines"
            applyPatches
        , test "new line" <|
            \_ ->
                applyPatches
                    1
                    [ Insertion ( 0, 0 ) (fromString "1")
                    , Insertion ( 0, 1 ) (fromString "\n")
                    ]
        , test "debugging" <|
            \_ ->
                applyPatches
                    5
                    [ Insertion ( 0, 0 ) (fromString "\n")
                    , Deletion ( 0, 0 ) ( 0, 1 )
                    ]
        ]
