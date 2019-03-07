module TestFuzzyMatch exposing (suite)

import Expect
import Helper.Fuzzy exposing (..)
import Test exposing (..)


suite : Test
suite =
    let
        ls =
            [ "TODO.md"
            , "build"
            , "css"
            , "dist"
            , "elm-package.json"
            , "elm-stuff"
            , "fake"
            , "fake.fsx"
            , "favicon.ico"
            , "index.html"
            , "node_modules"
            , "package-lock.json"
            , "package.json"
            , "packages"
            , "paket-files"
            , "paket.dependencies"
            , "paket.lock"
            , "src"
            , "src-fs"
            , "src-js"
            , "start"
            , "start.js"
            , "tags"
            , "tests"
            , "buffer.elm"
            ]

        cases =
            [ { src = [ "a", "b" ]
              , target = "a"
              , result = [ { text = "a", matches = [ 0 ] } ]
              }
            , { src = [ "a", "b" ]
              , target = ""
              , result =
                    [ { text = "a", matches = [] }
                    , { text = "b", matches = [] }
                    ]
              }
            , { src = []
              , target = "x"
              , result =
                    []
              }
            , { src = ls
              , target = "src"
              , result =
                    [ { text = "src"
                      , matches = [ 0, 1, 2 ]
                      }
                    , { text = "src-fs"
                      , matches = [ 0, 1, 2 ]
                      }
                    , { text = "src-js"
                      , matches = [ 0, 1, 2 ]
                      }
                    ]
              }
            , { src = ls
              , target = "buf"
              , result =
                    [ { text = "buffer.elm"
                      , matches = [ 0, 1, 2 ]
                      }
                    ]
              }
            , { src = [ "buffer.elm" ]
              , target = "buff"
              , result =
                    [ { text = "buffer.elm"
                      , matches = [ 0, 1, 2, 3 ]
                      }
                    ]
              }
            ]
    in
    describe "Fuzzy Match"
        (List.map
            (\c ->
                let
                    { src, target, result } =
                        c
                in
                test ("match `" ++ target ++ "`") <|
                    \_ ->
                        Expect.equal result (fuzzyMatch src target)
            )
            --(List.filter (\c -> c.target == "buff") cases)
            cases
        )
