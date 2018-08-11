module TestViewLines exposing (..)

import Fuzz
import Expect exposing (Expectation)
import Test exposing (..)
import Update.Buffer exposing (..)
import TextBuffer exposing (..)
import Model exposing (Buffer, emptyBuffer, ViewLine)
import Internal.TextBuffer exposing (Patch(..), fromString)


resize : Buffer -> Buffer
resize buf =
    let
        view =
            buf.view
    in
        { buf
            | view =
                { view
                    | size = { height = 5, width = 1 }
                    , lines = fillEmptyViewLines 5 view.lines
                }
        }


ignoreSyntax : ViewLine -> ViewLine
ignoreSyntax viewLine =
    { viewLine | tokens = [] }


applyPatches : List Patch -> Expectation
applyPatches patches =
    let
        buf =
            emptyBuffer
                |> resize
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
        [ fuzz (Fuzz.list fuzzPatch)
            "sorted viewLines should always equal to lines"
            applyPatches
        , test "debugging" <|
            \_ ->
                applyPatches
                    [ Insertion ( 0, 0 ) (fromString "\n")
                    , Deletion ( 0, 0 ) ( 0, 1 )
                    ]
        ]
