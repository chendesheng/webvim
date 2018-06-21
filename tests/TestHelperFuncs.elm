module TestHelperFuncs exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Helper.Helper exposing (..)


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
                , testUnix "/a/b/" "/a/c/d.elm" "../c/d.elm"
                , testUnix "/a/c/d.elm" "/a/b/" "../b/"
                , testWin "c:\\a\\b\\" "c:\\c\\d\\" "..\\..\\c\\d\\"
                , testWin "c:\\" "c:\\c\\d\\" "c\\d\\"
                , testWin "c:\\a.elm" "c:\\c\\d\\" "c\\d\\"
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
                ]
            )
        ]
