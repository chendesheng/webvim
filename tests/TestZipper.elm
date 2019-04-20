module TestZipper exposing (suite)

import Expect
import Helper.Helper exposing (repeatfn)
import Test exposing (..)
import Zipper exposing (Zipper)


createZipper : List Int -> List Int -> Zipper Int
createZipper backwards forwards =
    Zipper.fromList (backwards ++ forwards)
        |> repeatfn (List.length backwards) Zipper.moveForward
        |> Maybe.withDefault Zipper.empty


suite : Test
suite =
    describe "Test Zipper"
        [ test "moveForward on empty zipper" <|
            \_ ->
                Expect.equal (Zipper.empty |> Zipper.moveForward) Nothing
        , test "moveBackward on empty zipper" <|
            \_ ->
                Expect.equal (Zipper.empty |> Zipper.moveBackward) Nothing
        , test "moveForward" <|
            \_ ->
                Expect.equal
                    (Zipper.fromList [ 1, 2, 3 ]
                        |> Zipper.moveForward
                        |> Maybe.andThen Zipper.moveForward
                        |> Maybe.withDefault Zipper.empty
                    )
                    (createZipper [ 1, 2 ] [ 3 ])
        , test "moveForward to end" <|
            \_ ->
                Expect.equal
                    (Zipper.fromList [ 1, 2, 3 ]
                        |> Zipper.moveForward
                        |> Maybe.andThen Zipper.moveForward
                        |> Maybe.withDefault Zipper.empty
                        |> Zipper.moveForward
                    )
                    Nothing
        , test "moveBackward to head" <|
            \_ ->
                Expect.equal
                    (Zipper.fromList [ 1, 2, 3 ]
                        |> Zipper.moveBackward
                    )
                    Nothing
        , test "moveForward and moveBackward" <|
            \_ ->
                Expect.equal
                    (Zipper.fromList [ 1, 2, 3 ]
                        |> Zipper.moveForward
                        |> Maybe.andThen Zipper.moveBackward
                        |> Maybe.withDefault Zipper.empty
                    )
                    (Zipper.fromList [ 1, 2, 3 ])
        , test "moveBackward and moveForward" <|
            \_ ->
                Expect.equal
                    (Zipper.fromList [ 1, 2, 3 ]
                        |> Zipper.moveForward
                        |> Maybe.andThen Zipper.moveBackward
                        |> Maybe.andThen Zipper.moveForward
                        |> Maybe.withDefault Zipper.empty
                    )
                    (createZipper [ 1 ] [ 2, 3 ])
        , describe "getCurrent"
            [ test "empty" <|
                \_ -> Expect.equal (Zipper.getCurrent Zipper.empty) Nothing
            , test "not empty" <|
                \_ ->
                    Expect.equal (createZipper [] [ 1, 2 ] |> Zipper.getCurrent)
                        (Just 1)
            ]
        , describe "getNth"
            [ test "empty" <|
                \_ -> Expect.equal (Zipper.getNth 0 Zipper.empty) Nothing
            , test "not empty n=0" <|
                \_ ->
                    Expect.equal (createZipper [] [ 1, 2 ] |> Zipper.getNth 0)
                        (Just 1)
            , test "not empty n=1" <|
                \_ ->
                    Expect.equal (createZipper [] [ 1, 2 ] |> Zipper.getNth 1)
                        (Just 2)
            , test "not empty n=length" <|
                \_ ->
                    Expect.equal (createZipper [] [ 1, 2 ] |> Zipper.getNth 2)
                        Nothing
            , test "not empty n>length" <|
                \_ ->
                    Expect.equal (createZipper [] [ 1, 2 ] |> Zipper.getNth 3)
                        Nothing
            ]
        , describe "moveToHead"
            [ test "empty" <|
                \_ ->
                    Expect.equal (Zipper.moveToHead Zipper.empty) Zipper.empty
            , test "not empty" <|
                \_ ->
                    Expect.equal (createZipper [ 1, 4 ] [ 2, 3 ] |> Zipper.moveToHead)
                        (createZipper [] [ 1, 4, 2, 3 ])
            ]
        ]
