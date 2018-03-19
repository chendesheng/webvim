module TestPositionClass exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import PositionClass exposing (..)
import Vim.AST exposing (PositionClass(..), Direction(..))
type TestCase
    = TestCase PositionClass Direction String


suite : Test
suite =
    describe "single line" <|
        let
            cases =
                List.map (TestCase WordStart Forward)
                    [ """
123
^
"""
                    , """
12 c
^  ?
"""
                    , """
12 c
 ^ ?
"""
                    , """
## c
^  ?
"""
                    , """
# %
^ ?
"""
                    , """
abc# 123
^  ?
"""
                    , """
  12
^ ?
"""
                    , """
  ?
^ ?
"""
                    , """
 ?
^?
"""
                    , """
 c
^?
"""
                    , """
\t\t
^
"""
                    ]
                    ++ List.map (TestCase WordEnd Forward)
                        [ """
123
^ ?
"""
                        , """
12 3
^?
"""
                        , """
1ab2#
^  ?
"""
                        , """
1 ab2
^   ?
"""
                        , """
1 #
^ ?
"""
                        , """
  ##
^  ?
"""
                        , """
 #
^?
"""
                        , """
13#
 ^?
"""
                        , """
\t
^
"""
                        ]
                    ++ List.map (TestCase WORDStart Forward)
                        [ """
123##bb
^
"""
                        , """
123##bb 1
^       ?
"""
                        , """
xxyy#? %
 ^     ?
"""
                        , """
xxyy#? a
 ^     ?
"""
                        , """
\t
^
"""
                        , """
    12
  ^ ?
"""
                        ]
                    ++ List.map (TestCase WORDEnd Forward)
                        [ """
123ab$$##
^       ?
"""
                        , """
1   #
^   ?
"""
                        , """
1   #
  ^ ?
"""
                        , """
1ef   #123
  ^      ?
"""
                        , """
\t
^
"""
                        , """
\t
   ^
"""
                        , """
ah
^?
"""
                        , """
 h
^?
"""
                        ]
        in
            List.map
                (\(TestCase class direction testcase) ->
                    test
                        (String.join " "
                            [ toString class, toString direction, testcase ]
                        )
                    <|
                        \_ ->
                            case String.lines testcase of
                                [ _, line, cursor, _ ] ->
                                    let
                                        start =
                                            String.indexes "^" cursor
                                                |> List.head
                                                |> Maybe.withDefault 0

                                        result =
                                            String.indexes "?" cursor
                                                |> List.head
                                    in
                                        Expect.equal
                                            (findPosition ""
                                                class
                                                direction
                                                line
                                                start
                                            )
                                            result

                                _ ->
                                    Expect.fail "wrong test case"
                )
                cases
