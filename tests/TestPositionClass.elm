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
    = TestCase MotionData String String


suite : Test
suite =
    describe "single line" <|
        let
            cases =
                List.map (TestCase WordStart ">]+-")
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
                    ++ List.map (TestCase WordStart "<]+-")
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
                    ++ List.map (TestCase WordEnd ">]+-")
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
                    ++ List.map (TestCase WordEnd "<]+-")
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
                    ++ List.map (TestCase WORDStart ">]+-")
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
                    ++ List.map (TestCase WORDStart "<]+-")
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
                    ++ List.map (TestCase WORDEnd ">]+-")
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
                    ++ List.map (TestCase WORDEnd "<]+-")
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
                    ++ List.map (TestCase LineStart "<]$-")
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
                    ++ List.map (TestCase LineFirst "<]$-")
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
                    ++ List.map (TestCase LineEnd ">]$-")
                        [ """
123ab$$##
^        ?
"""
                        , """
1   #
^    ?
"""
                        , """
  1   #
^      ?
"""
                        ]
                    ++ List.map (TestCase WordEdge ">]$=")
                        [ """
    a
^   ?
"""
                        , """
\t
^?
"""
                        , """
\t\t
^ ?
"""
                        , """
123a##
^   ?
"""
                        , """
123 ##
^  ?
"""
                        , """
#1
^?
"""
                        , """
# 1
^?
"""
                        , """
  #
^ ?
"""
                        , """
  1
^ ?
"""
                        , """
1
^?
"""
                        , """
#
^?
"""
                        ]
                    ++ List.map (TestCase WORDEdge ">]$-")
                        [ """
123a
^  ?
"""
                        , """
  123a
^?
"""
                        ]
                    ++ List.map (TestCase (MatchChar "3" True) ">]$-")
                        [ """
123a
^?
"""
                        , """
12k3a
^ ?
"""
                        , """
12a
^
"""
                        ]
                    ++ List.map (TestCase (MatchChar "1" True) "<]$-")
                        [ """
123a
 ?^
"""
                        , """
12k3a
 ? ^
"""
                        , """
32a
^
"""
                        ]
                    ++ List.map (TestCase (MatchChar "3" False) ">]$-")
                        [ """
123a
^ ?
"""
                        , """
12k3a
^  ?
"""
                        , """
12a
^
"""
                        ]
                    ++ List.map (TestCase (MatchChar "1" False) "<]$-")
                        [ """
123a
? ^
"""
                        , """
12k3a
?  ^
"""
                        , """
32a
^
"""
                        , """
32a
 ^
"""
                        ]
                    ++ List.map (TestCase CharStart ">]$-")
                        [ """
123a
^?
"""
                        , """
12k3a
^?
"""
                        , """
1
^?
"""
                        ]
                    ++ List.map (TestCase CharStart "<]$-")
                        [ """
123a
?^
"""
                        , """
12k3a
?^
"""
                        , """
1
^
"""
                        ]
                    ++ List.map (TestCase WordStart ">)$-")
                        [ """
1  23a
^  ?
"""
                        , """
1#
^?
"""
                        , """
#1
^?
"""
                        , """
# 1
^ ?
"""
                        , """
1
^?
"""
                        ]
                    ++ List.map (TestCase WORDStart ">)$-")
                        [ """
1  23a
^  ?
"""
                        , """
1#
^ ?
"""
                        , """
#1
^ ?
"""
                        , """
# 1
^ ?
"""
                        , """
1
^?
"""
                        ]
        in
            List.map
                (\(TestCase md option testcase) ->
                    test
                        (String.join " "
                            [ toString md, option, testcase ]
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

                                        mo =
                                            motionOption option
                                    in
                                        Expect.equal
                                            (findPosition ""
                                                md
                                                mo
                                                line
                                                start
                                            )
                                            result

                                _ ->
                                    Expect.fail "wrong test case"
                )
                cases
