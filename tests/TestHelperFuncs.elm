module TestHelperFuncs exposing (suite)

import Array as Array exposing (Array)
import Expect exposing (Expectation)
import Helper.Helper exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Helper functions"
        [ describe "relativePath"
            (let
                testRelativePath sep from to result =
                    test (from ++ " -> " ++ to)
                        (\_ ->
                            Expect.equal result <| relativePath sep from to
                        )

                testUnix =
                    testRelativePath "/"

                testWin =
                    testRelativePath "\\"
             in
             [ testUnix
                "/users/webvim/"
                "/users/webvim/src/main.elm"
                "src/main.elm"
             , testUnix
                "/users/me/webvim"
                "/users/me/.elm/0.19/main.elm"
                "../.elm/0.19/main.elm"
             , testUnix "/a/b/" "/a/c/d.elm" "../c/d.elm"
             , testWin "c:\\a\\b\\" "c:\\c\\d\\" "..\\..\\c\\d\\"
             , testWin "c:\\" "c:\\c\\d\\" "c\\d\\"
             , testWin "c:\\a\\b" "c:\\c\\d\\" "..\\..\\c\\d\\"
             ]
            )
        , describe "resolvePath"
            (let
                testResolvePath sep dir path result =
                    test (dir ++ " ++ " ++ path)
                        (\_ ->
                            Expect.equal result <| resolvePath sep dir path
                        )

                testUnix =
                    testResolvePath "/"

                testWin =
                    testResolvePath "\\"
             in
             [ testUnix "/a/b/c" "d/e/f" "/a/b/c/d/e/f"
             , testUnix "/a/b/c/" "d/e/f" "/a/b/c/d/e/f"
             , testUnix "/a" "../" "/"
             , testUnix "/a" "../../" ""
             , testUnix "/a" "../f.x" "/f.x"
             , testUnix "/a/b/c" "/absolute" "/absolute"
             , testUnix "/a/b/c" "../d" "/a/b/d"
             , testUnix "/a/b/c" "../../d" "/a/d"
             , testUnix "" "src/Main.elm" "src/Main.elm"
             , testWin "c:\\windows" "system32" "c:\\windows\\system32"
             , testWin "c:\\users\\chendesheng\\webvim" "src\\main.elm" "c:\\users\\chendesheng\\webvim\\src\\main.elm"
             ]
            )
        , describe "normalizePath"
            (let
                testNormalizePath sep path result =
                    test path
                        (\_ ->
                            Expect.equal result <| normalizePath sep path
                        )

                testUnix =
                    testNormalizePath "/"

                testWin =
                    testNormalizePath "\\"
             in
             [ testUnix "\\d/e/f" "/d/e/f"
             , testWin "\\d/e\\f  " "\\d\\e\\f"
             , testWin "\\\\\\d/e\\f  " "\\d\\e\\f"
             , testWin "\\\\/\\\\d/e\\f  " "\\d\\e\\f"
             ]
            )
        , describe "nthList"
            [ test "nthList 0 [0]" <|
                \_ ->
                    Expect.equal (Just 0) (nthList 0 [ 0 ])
            , test "nthList 0 []" <|
                \_ ->
                    Expect.equal Nothing (nthList 0 [])
            , test "nthList 1 [0, 1]" <|
                \_ ->
                    Expect.equal (Just 1) (nthList 1 [ 0, 1 ])
            , test "nthList 2 [0, 1]" <|
                \_ ->
                    Expect.equal Nothing (nthList 2 [ 0, 1 ])
            ]
        , describe "arrayInsert"
            (let
                testArrayInsert n item target result =
                    test
                        (String.fromInt n
                            ++ " "
                            ++ String.fromInt item
                            ++ " "
                            ++ Debug.toString target
                        )
                    <|
                        \_ ->
                            target
                                |> Array.fromList
                                |> arrayInsert n item
                                |> Array.toList
                                |> Expect.equal result
             in
             [ testArrayInsert 0 0 [] [ 0 ]
             , testArrayInsert 0 0 [ 1 ] [ 0, 1 ]
             , testArrayInsert 1 0 [ 1 ] [ 1, 0 ]
             , testArrayInsert 1 0 [ 1, 2, 3 ] [ 1, 0, 2, 3 ]
             ]
            )
        , describe "filename"
            (let
                testFilename name result =
                    test name
                        (\_ ->
                            Expect.equal result <| filename name
                        )
             in
             [ testFilename "a" ( "a", "" )
             , testFilename "a.b" ( "a", ".b" )
             , testFilename "a.b.c" ( "a.b", ".c" )
             , testFilename "a/b/c" ( "c", "" )
             , testFilename "a\\b\\c.d" ( "c", ".d" )
             , testFilename "a/b\\c.d.e" ( "c.d", ".e" )
             ]
            )
        , describe "findIndex"
            (let
                testFindIndex arr i result =
                    test ("find " ++ String.fromInt i ++ " in " ++ Debug.toString arr)
                        (\_ ->
                            Expect.equal result <| findIndex ((==) i) arr
                        )
             in
             [ testFindIndex [] 0 Nothing
             , testFindIndex [ 0 ] 0 <| Just 0
             , testFindIndex [ 1, 2 ] 0 Nothing
             , testFindIndex [ 1, 2 ] 1 <| Just 0
             ]
            )
        ]
