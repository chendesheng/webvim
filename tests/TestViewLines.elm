module TestViewLines exposing (..)

import Fuzz
import Expect exposing (Expectation)
import Test exposing (..)
import Update.Buffer exposing (..)
import TextBuffer exposing (..)
import Model exposing (Buffer, emptyBuffer, ViewLine)
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
                    , lines = fillEmptyViewLines height view.lines
                }
        }


ignoreSyntax : ViewLine -> ViewLine
ignoreSyntax viewLine =
    { viewLine | tokens = [] }


applyPatches : Int -> List Patch -> Expectation
applyPatches height patches =
    let
        buf =
            emptyBuffer
                |> resize height
                |> transaction patches
    in
        Expect.equal
            (getViewLines buf.view.scrollTop
                (buf.view.scrollTop + buf.view.size.height + 2)
                buf.lines
                buf.syntax
                |> List.filterMap (Maybe.map ignoreSyntax)
            )
            (buf.view.lines
                |> List.filterMap (Maybe.map ignoreSyntax)
                |> List.sortBy .lineNumber
            )


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
