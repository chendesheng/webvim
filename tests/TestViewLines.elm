module TestViewLines exposing (applyPatches, resize, suite)

import Expect exposing (Expectation)
import Fuzz
import Internal.TextBuffer exposing (Patch(..), fromString)
import Model
    exposing
        ( Buffer
        , Editor
        , emptyBuffer
        , emptyGlobal
        , updateBuffer
        )
import Test exposing (..)
import TextBuffer exposing (..)
import Update.Buffer exposing (..)


resize : Int -> Editor -> Editor
resize height ({ buf, global } as ed) =
    let
        view =
            buf.view
    in
    { ed
        | buf =
            { buf
                | view =
                    { view
                        | lines = List.range 0 (height + 1)
                        , size = { height = height, width = 1 }
                    }
            }
    }


applyPatches : Int -> List Patch -> Expectation
applyPatches height patches =
    let
        { buf, global } =
            { buf = emptyBuffer
            , global = emptyGlobal
            }
                |> resize height
                |> updateBuffer (transaction patches)
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
