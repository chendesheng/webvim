module TestTextObject exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Internal.TextBuffer as B
import TextObject exposing (expandTextObject)
import Vim.AST
    exposing
        ( MotionData(..)
        , MotionOption
        , motionOption
        , Direction(..)
        , TextObject(..)
        )


type TestCase
    = TestCase TextObject Bool String


suite : Test
suite =
    describe "single line" <|
        let
            cases =
                List.map (TestCase Word False)
                    [ """
123
^
? ?
"""
                    , """
123
 ^
? ?
"""
                    , """
123
  ^
? ?
"""
                    , """
2
^
?
"""
                    , """
 2
^
?
"""
                    , """
2   2
 ^
 ? ?
"""
                    , """
 $%^
 ^
 ? ?
"""
                    ]
        in
            List.map
                (\(TestCase textobj around testcase) ->
                    test
                        (String.join " "
                            [ toString textobj, toString around, testcase ]
                        )
                    <|
                        \_ ->
                            case String.lines testcase of
                                [ _, line, cursor, result, _ ] ->
                                    let
                                        range =
                                            case String.indexes "?" result of
                                                [ a ] ->
                                                    Just ( ( 0, a ), ( 0, a + 1 ) )

                                                a :: b :: rest ->
                                                    Just ( ( 0, a ), ( 0, b + 1 ) )

                                                _ ->
                                                    Nothing

                                        start =
                                            String.indexes "^" cursor
                                                |> List.head
                                                |> Maybe.withDefault 0
                                    in
                                        Expect.equal
                                            range
                                            (expandTextObject
                                                ""
                                                textobj
                                                around
                                                (B.fromString line)
                                                ( 0, start )
                                            )

                                _ ->
                                    Expect.fail "wrong test case"
                )
                cases
