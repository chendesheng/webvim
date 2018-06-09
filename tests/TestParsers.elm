module TestParsers exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Update.Service exposing (syntaxErrorParser)


suite : Test
suite =
    describe "Parsers"
        [ test "syntaxErrorParser" <|
            \_ ->
                let
                    resp =
                        "-- SYNTAX PROBLEM ------------------------------------------------- src/View.elm\n\nI need whitespace, but got stuck on what looks like a new declaration. You are\neither missing some stuff in the declaration above or just need to add some\nspaces here:\n\n31| rem : number -> String\n    ^\nI am looking for one of the following things:\n\n    whitespace\n\n"
                in
                    Expect.equal
                        (Ok
                            { tipe = "error"
                            , tag = "SYNTAX PROBLEM"
                            , file = "src/View.elm"
                            , overview = "I need whitespace, but got stuck on what looks like a new declaration. You are\neither missing some stuff in the declaration above or just need to add some\nspaces here:"
                            , details = "I am looking for one of the following things:\n\n    whitespace"
                            , region = ( ( 30, 0 ), ( 30, 0 ) )
                            , subRegion = Nothing
                            }
                        )
                        (P.run syntaxErrorParser resp)
        ]
