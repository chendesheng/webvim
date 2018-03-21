module TestPositionClass exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import PositionClass exposing (..)
import Vim.AST
    exposing
        ( MotionData(..)
        , MotionOption
        , motionOption
        , Direction(..)
        )


isEven : Int -> Bool
isEven x =
    x % 2 == 0


isOdd : Int -> Bool
isOdd x =
    not <| isEven x


filterByIndex : (Int -> Bool) -> List a -> List a
filterByIndex pred lst =
    lst
        |> List.indexedMap
            (\i a ->
                if pred i then
                    Just a
                else
                    Nothing
            )
        |> List.filterMap identity


type TestCase
    = TestCase MotionData Direction String


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
                    ++ List.map (TestCase WordStart Backward)
                        [ """
123
? ^
"""
                        , """
12 3
?^
"""
                        , """
1ab2#
?  ^
"""
                        , """
1 ab2
  ? ^
"""
                        , """
1 #
? ^
"""
                        , """
  ##
  ?^
"""
                        , """
 #
 ^
"""
                        , """
13#
? ^
"""
                        , """
\t
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
                    ++ List.map (TestCase WordEnd Backward)
                        [ """
123
^
"""
                        , """
12 c
 ? ^
"""
                        , """
## c
 ? ^
"""
                        , """
## 1
 ? ^
"""
                        , """
# %
? ^
"""
                        , """
abc#
  ?^
"""
                        , """
  12
  ^
"""
                        , """
?  1
? ^
"""
                        , """
 ?
 ^
"""
                        , """
\t\t
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
                    ++ List.map (TestCase WORDStart Backward)
                        [ """
123##bb
?   ^
"""
                        , """
123##bb 1
?       ^
"""
                        , """
%xyy#? x
?      ^
"""
                        , """
xxyy#?
?  ^
"""
                        , """
\t
^
"""
                        , """
    12
    ^
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
                    ++ List.map (TestCase WORDEnd Backward)
                        [ """
123ab$$##
        ^
"""
                        , """
1   #
?   ^
"""
                        , """
1   #
?  ^
"""
                        , """
1ef   #123
  ?      ^
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
a h
? ^
"""
                        , """
 h
 ^
"""
                        ]
                    ++ List.map (TestCase LineStart Backward)
                        [ """
123ab$$##
?       ^
"""
                        , """
1   #
?   ^
"""
                        , """
  1   #
?  ^
"""
                        , """
1   #123
?       ^
"""
                        ]
                    ++ List.map (TestCase LineFirst Backward)
                        [ """
123ab$$##
?       ^
"""
                        , """
1   #
?   ^
"""
                        , """
  1   #
  ?^
"""
                        , """
  1   #123
^ ?
"""
                        ]
                    ++ List.map (TestCase LineEnd Forward)
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
^     ?
"""
                        ]
                    ++ List.map (TestCase WordEdge Forward)
                        [ """
123a##
^  ?
"""
                        , """
123 ##
^ ?
"""
                        , """
#1
$
"""
                        , """
# 1
$
"""
                        , """
  #
^?
"""
                        , """
  1
^?
"""
                        , """
1
$
"""
                        , """
#
$
"""
                        ]
                    ++ List.map (TestCase WORDEdge Forward)
                        [ """
123a
^  ?
"""
                        , """
  123a
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
                                        both =
                                            String.indexes "$" cursor
                                                |> List.head

                                        start =
                                            case both of
                                                Just n ->
                                                    n

                                                _ ->
                                                    String.indexes "^" cursor
                                                        |> List.head
                                                        |> Maybe.withDefault 0

                                        result =
                                            case both of
                                                Just n ->
                                                    Just n

                                                _ ->
                                                    String.indexes "?" cursor
                                                        |> List.head

                                        option =
                                            motionOption ">]+="
                                    in
                                        Expect.equal
                                            (findPosition ""
                                                class
                                                { option
                                                    | forward =
                                                        direction == Forward
                                                }
                                                line
                                                start
                                            )
                                            result

                                _ ->
                                    Expect.fail "wrong test case"
                )
                cases
