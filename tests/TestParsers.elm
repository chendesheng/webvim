module TestParsers exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Update.Service exposing (syntaxErrorParser)
import Internal.PositionClass
    exposing
        ( parserForwardCharRespectBackslash
        , parserBackwardCharRespectBackslash
        )


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
        , describe "forwardCharRespectBackslash" <|
            let
                testParser s expect =
                    test ("find in " ++ s) <|
                        \_ ->
                            s
                                |> P.run
                                    (parserForwardCharRespectBackslash
                                        '"'
                                    )
                                |> expect
            in
                [ testParser "abc\"def" <| Expect.equal (Ok 3)
                , testParser "abc\\\"d\"ef" <| Expect.equal (Ok 6)
                , testParser "\"" <| Expect.equal (Ok 0)
                , testParser "\\\"\"" <| Expect.equal (Ok 2)
                , testParser "abc\\\"def" Expect.err
                , testParser "abcdef" Expect.err
                , testParser "\n" Expect.err
                , testParser "\\" Expect.err
                ]
        , describe "backwardCharRespectBackslash" <|
            let
                testParser s expect =
                    test ("find in " ++ s) <|
                        \_ ->
                            s
                                |> String.reverse
                                |> P.run
                                    (parserBackwardCharRespectBackslash
                                        '"'
                                    )
                                |> expect
            in
                [ testParser "a\"" <| Expect.equal (Ok 0)
                , testParser "\"\\a" <| Expect.equal (Ok 2)
                , testParser "\"a\\\"" <| Expect.equal (Ok 3)
                , testParser "\"\"" <| Expect.equal (Ok 0)
                , testParser "\"" <| Expect.equal (Ok 0)
                , testParser "\\\"" Expect.err
                , testParser "\\\"aa" Expect.err
                ]
        ]
