module VimParser exposing (..)

import Expect exposing (Expectation)
import Vim.Parser as V
import Vim.AST exposing (..)


-- import Fuzz exposing (Fuzzer, int, list, string)

import Test exposing (..)


cases : List ( String, AST )
cases =
    [ ( "w"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
          }
        , ""
        )
      )
    , ( "b"
      , ( { initialMode
            | edit =
                { direction = Backward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
          }
        , ""
        )
      )

    -- 'g' operator
    , ( "g"
      , ( initialMode, "g" )
      )
    , ( "gg"
      , ( { initialMode
            | edit = LineNumber 0 |> Move |> Just
          }
        , ""
        )
      )
    , ( "g<esc>"
      , ( initialMode, "" )
      )
    , ( "gj"
      , ( { initialMode
            | edit = VLineDelta 1 |> Move |> Just
          }
        , ""
        )
      )
    , ( "gJ"
      , ( { initialMode
            | edit = Join True |> Just
          }
        , ""
        )
      )

    -- insert
    , ( "i"
      , ( { initialMode | mode = ModeNameInsert }, "i" )
      )
    , ( "i<esc>"
      , ( { initialMode | mode = ModeNameNormal }, "" )
      )
    , ( "ia"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit = InsertString "a" |> Just
          }
        , "i"
        )
      )
    , ( "c", ( initialMode, "c" ) )
    , ( "c<esc>", ( initialMode, "" ) )
    , ( "cw"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> MotionRange Exclusive
                    |> Delete
                    |> Just
          }
        , "cw"
        )
      )
    , ( "cv", ( initialMode, "cv" ) )
    , ( "cv<esc>", ( initialMode, "" ) )
    , ( "cvw"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> MotionRange Inclusive
                    |> Delete
                    |> Just
          }
        , "cvw"
        )
      )
    , ( "cw<esc>", ( initialMode, "" ) )
    , ( "cvw<esc>", ( initialMode, "" ) )
    , ( "cwa"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit = InsertString "a" |> Just
          }
        , "cw"
        )
      )
    , ( "cawa"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit = InsertString "a" |> Just
          }
        , "caw"
        )
      )
    , ( "C"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit =
                { direction = Forward, class = LineEnd }
                    |> ByClass
                    |> MotionRange Exclusive
                    |> Delete
                    |> Just
          }
        , "C"
        )
      )
    , ( "Cw"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit = InsertString "w" |> Just
          }
        , "C"
        )
      )

    -- range operator
    , ( "d", ( initialMode, "d" ) )
    , ( "d<esc>", ( initialMode, "" ) )
    , ( "dw"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> MotionRange Exclusive
                    |> Delete
                    |> Just
          }
        , ""
        )
      )
    , ( "dv", ( initialMode, "dv" ) )
    , ( "dvw"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> MotionRange Inclusive
                    |> Delete
                    |> Just
          }
        , ""
        )
      )
    , ( "\\<", ( initialMode, "\\<" ) )
    , ( "\\<<esc>", ( initialMode, "" ) )
    , ( "\\<w"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> MotionRange Exclusive
                    |> Indent Backward
                    |> Just
          }
        , ""
        )
      )
    , ( "\\<v", ( initialMode, "\\<v" ) )
    , ( "\\<vw"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> MotionRange Inclusive
                    |> Indent Backward
                    |> Just
          }
        , ""
        )
      )
    , ( "J"
      , ( { initialMode | edit = Join False |> Just }, "" )
      )

    -- ex
    , ( ":"
      , ( { initialMode | mode = ModeNameEx ":" }, ":" )
      )
    , ( ":<esc>"
      , ( initialMode, "" )
      )
    , ( ":<cr>"
      , ( { initialMode | edit = ExecuteLine |> Just }, "" )
      )
    , ( ":a"
      , ( { initialMode
            | edit = InsertString "a" |> Just
            , mode = ModeNameEx ":"
          }
        , ":"
        )
      )
    , ( "/"
      , ( { initialMode | mode = ModeNameEx "/" }, "/" )
      )
    , ( "/<esc>"
      , ( initialMode, "" )
      )
    , ( "/<cr>"
      , ( { initialMode | edit = MatchString Forward |> Move |> Just }, "" )
      )
    , ( "/a"
      , ( { initialMode
            | edit = InsertString "a" |> Just
            , mode = ModeNameEx "/"
          }
        , "/"
        )
      )
    , ( "?"
      , ( { initialMode | mode = ModeNameEx "?" }, "?" )
      )
    , ( "?<esc>"
      , ( initialMode, "" )
      )
    , ( "?<cr>"
      , ( { initialMode | edit = MatchString Backward |> Move |> Just }, "" )
      )
    , ( "?a"
      , ( { initialMode
            | edit = InsertString "a" |> Just
            , mode = ModeNameEx "?"
          }
        , "?"
        )
      )
    , ( "?<c-h>"
      , ( { initialMode
            | edit =
                { direction = Backward, class = CharStart }
                    |> ByClass
                    |> MotionRange Inclusive
                    |> Delete
                    |> Just
            , mode = ModeNameEx "?"
          }
        , "?"
        )
      )

    -- temp normal
    , ( "i<c-o>"
      , ( { initialMode | mode = ModeNameTempNormal }, "i<c-o>" )
      )
    , ( "i<c-o><esc>"
      , ( { initialMode | mode = ModeNameInsert }, "i" )
      )
    , ( "i<c-o>w"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
            , mode = ModeNameInsert
          }
        , "i"
        )
      )
    , ( "i<c-o>i"
      , ( { initialMode | mode = ModeNameInsert }, "i" )
      )
    , ( "i<c-o>c"
      , ( { initialMode | mode = ModeNameTempNormal }
        , "i<c-o>c"
        )
      )
    , ( "i<c-o>ce"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordEnd }
                    |> ByClass
                    |> MotionRange Exclusive
                    |> Delete
                    |> Just
            , mode = ModeNameInsert
          }
        , "i"
        )
      )
    , ( "cw<c-o>"
      , ( { initialMode | mode = ModeNameTempNormal }, "cw<c-o>" )
      )
    , ( "cw<c-o><esc>"
      , ( { initialMode | mode = ModeNameInsert }, "cw" )
      )
    , ( "cw<c-o>w"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
            , mode = ModeNameInsert
          }
        , "cw"
        )
      )
    , ( "cw<c-o>i"
      , ( { initialMode | mode = ModeNameInsert }, "cw" )
      )
    , ( "cw<c-o>c"
      , ( { initialMode | mode = ModeNameTempNormal }
        , "cw<c-o>c"
        )
      )
    , ( "cw<c-o>ce"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordEnd }
                    |> ByClass
                    |> MotionRange Exclusive
                    |> Delete
                    |> Just
            , mode = ModeNameInsert
          }
        , "cw"
        )
      )

    -- record macro
    , ( "q"
      , ( initialMode, "q" )
      )
    , ( "q<esc>", ( initialMode, "" ) )
    , ( "qa"
      , ( { initialMode | recordMacro = Just "a" }, "qa" )
      )
    , ( "qa<esc>"
      , ( { initialMode | recordMacro = Just "a" }, "qa" )
      )
    , ( "qaw"
      , ( { initialMode
            | recordMacro = Just "a"
            , edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
          }
        , "qa"
        )
      )
    , ( "qac"
      , ( { initialMode | recordMacro = Just "a" }, "qac" )
      )
    , ( "qacw"
      , ( { initialMode
            | recordMacro = Just "a"
            , mode = ModeNameInsert
            , edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> MotionRange Exclusive
                    |> Delete
                    |> Just
          }
        , "qacw"
        )
      )
    , ( "qacwq"
      , ( { initialMode
            | recordMacro = Just "a"
            , mode = ModeNameInsert
            , edit = InsertString "q" |> Just
          }
        , "qacw"
        )
      )
    , ( "qaq"
      , ( { initialMode | recordMacro = Nothing }, "" )
      )

    -- register
    , ( "\""
      , ( initialMode, "\"" )
      )
    , ( "\"a"
      , ( { initialMode | register = "a" }, "\"a" )
      )
    , ( "\"a\""
      , ( { initialMode | register = "a" }, "\"a\"" )
      )
    , ( "\"a\"b"
      , ( { initialMode | register = "b" }, "\"a\"b" )
      )
    , ( "\"<esc>"
      , ( initialMode, "" )
      )
    , ( "\"aw"
      , ( { initialMode
            | register = "a"
            , edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
          }
        , ""
        )
      )
    , ( "i<c-r>a"
      , ( { initialMode
            | register = "a"
            , mode = ModeNameInsert
            , edit = Put Forward |> Just
          }
        , "i"
        )
      )

    --count
    , ( "2"
      , ( { initialMode | count = 2 }, "2" )
      )
    , ( "23"
      , ( { initialMode | count = 23 }, "23" )
      )
    , ( "23<esc>", ( initialMode, "" ) )
    , ( "23\"a24"
      , ( { initialMode
            | register = "a"
            , count = 23 * 24
          }
        , "23\"a24"
        )
      )
    , ( "@", ( initialMode, "@" ) )
    , ( "@<esc>", ( initialMode, "" ) )
    , ( "@q"
      , ( { initialMode | edit = ReplayMacro "q" |> Just }, "" )
      )

    -- visual
    , ( "v"
      , ( { initialMode | mode = ModeNameVisual }, "v" )
      )
    , ( "V"
      , ( { initialMode | mode = ModeNameVisualLine }, "V" )
      )
    , ( "<c-v>"
      , ( { initialMode | mode = ModeNameVisualBlock }, "<c-v>" )
      )
    , ( "vV"
      , ( { initialMode | mode = ModeNameVisualLine }, "V" )
      )
    , ( "v<c-v>"
      , ( { initialMode | mode = ModeNameVisualBlock }, "<c-v>" )
      )
    , ( "Vv"
      , ( { initialMode | mode = ModeNameVisual }, "v" )
      )
    , ( "V<c-v>"
      , ( { initialMode | mode = ModeNameVisualBlock }, "<c-v>" )
      )
    , ( "vv", ( initialMode, "" ) )
    , ( "VV", ( initialMode, "" ) )
    , ( "<c-v><c-v>", ( initialMode, "" ) )
    , ( "vw"
      , ( { initialMode
            | mode = ModeNameVisual
            , edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
          }
        , "v"
        )
      )
    , ( "vi", ( { initialMode | mode = ModeNameVisual }, "vi" ) )
    , ( "viw"
      , ( { initialMode
            | mode = ModeNameVisual
            , edit = Select Word False |> Just
          }
        , "v"
        )
      )
    , ( "v12iw"
      , ( { initialMode
            | mode = ModeNameVisual
            , count = 12
            , edit = Select Word False |> Just
          }
        , "v"
        )
      )
    , ( "vd"
      , ( { initialMode
            | edit = Delete VisualRange |> Just
          }
        , ""
        )
      )
    , ( "vc"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit = Delete VisualRange |> Just
          }
        , "vc"
        )
      )
    , ( "vcx"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit = InsertString "x" |> Just
          }
        , "vc"
        )
      )
    , ( "vC"
      , ( { initialMode
            | mode = ModeNameInsert
            , edit =
                { direction = Forward, class = LineEnd }
                    |> ByClass
                    |> MotionRange Exclusive
                    |> Delete
                    |> Just
          }
        , "vC"
        )
      )

    -- template
    , ( ""
      , ( initialMode, "" )
      )
    ]


suite : Test
suite =
    describe "Vim key parser module"
        (List.map
            (\( keys, mode ) ->
                test ("Parse `" ++ keys ++ "`") <|
                    \_ ->
                        Expect.equal mode (V.parse keys "")
            )
            cases
        )
