module TestLint exposing (suite)

import Expect exposing (Expectation)
import Parser as P
import Test exposing (..)
import Update.Lint exposing (syntaxErrorParser)


suite : Test
suite =
    describe "parser lint response"
        [ test "elm syntax problem" <|
            \_ ->
                let
                    result =
                        P.run syntaxErrorParser """-- SYNTAX PROBLEM - /var/folders/q5/834v4zqd4ln2m3c43gnvvky40000gn/T/l29qce64.eg5

I ran into something unexpected when parsing your code!

28| import aaDocument as Doc
           ^
I am looking for one of the following things:

    an upper case name
    whitespace

"""

                    --|> Debug.log "result"
                in
                case result of
                    Ok _ ->
                        Expect.pass

                    Err err ->
                        Expect.fail (Debug.toString err)
        ]
