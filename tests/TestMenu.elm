module TestMenu exposing (suite)

import Expect
import Menu as Mu
import Test exposing (..)


testMenu =
    Mu.init 3 [ 1, 2, 3, 4 ]


repeatfn : Int -> (a -> a) -> a -> a
repeatfn n fn arg =
    if n == 0 then
        arg

    else
        repeatfn (n - 1) fn (fn arg)


suite : Test
suite =
    describe "Test Menu"
        [ describe "selectForward"
            [ test "1" <|
                \_ ->
                    Expect.all
                        [ Mu.getSelected >> Expect.equal (Just 1)
                        , Mu.show >> Expect.equal [ 1, 2, 3 ]
                        ]
                        (testMenu
                            |> Mu.selectForward
                        )
            , test "2" <|
                \_ ->
                    Expect.all
                        [ Mu.getSelected >> Expect.equal (Just 2)
                        , Mu.show >> Expect.equal [ 1, 2, 3 ]
                        ]
                        (testMenu
                            |> repeatfn 2 Mu.selectForward
                        )
            , test "3" <|
                \_ ->
                    Expect.all
                        [ Mu.getSelected >> Expect.equal (Just 3)
                        , Mu.show >> Expect.equal [ 1, 2, 3 ]
                        ]
                        (testMenu
                            |> repeatfn 3 Mu.selectForward
                        )
            , test "4" <|
                \_ ->
                    Expect.all
                        [ Mu.getSelected >> Expect.equal (Just 4)
                        , Mu.show >> Expect.equal [ 2, 3, 4 ]
                        ]
                        (testMenu
                            |> repeatfn 4 Mu.selectForward
                        )
            , test "5" <|
                \_ ->
                    Expect.all
                        [ Mu.getSelected >> Expect.equal Nothing
                        , Mu.show >> Expect.equal [ 2, 3, 4 ]
                        ]
                        (testMenu
                            |> repeatfn 5 Mu.selectForward
                        )
            , test "6" <|
                \_ ->
                    Expect.all
                        [ Mu.getSelected >> Expect.equal (Just 1)
                        , Mu.show >> Expect.equal [ 1, 2, 3 ]
                        ]
                        (testMenu
                            |> repeatfn 6 Mu.selectForward
                        )
            ]
        ]
