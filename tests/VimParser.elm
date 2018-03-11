module VimParser exposing (..)

import Expect exposing (Expectation)
import Vim.Parser as V
import Vim.AST exposing (..)


-- import Fuzz exposing (Fuzzer, int, list, string)

import Test exposing (..)


cases : List ( String, ( AST, String ) )
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
      , ( { initialMode | modeName = ModeNameInsert }, "i" )
      )
    , ( "i<esc>"
      , ( { initialMode | modeName = ModeNameNormal }, "" )
      )
    , ( "ia"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit = InsertString "a" |> Just
          }
        , "i"
        )
      )
    , ( "c", ( initialMode, "c" ) )
    , ( "c<esc>", ( initialMode, "" ) )
    , ( "cw"
      , ( { initialMode
            | modeName = ModeNameInsert
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
            | modeName = ModeNameInsert
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
            | modeName = ModeNameInsert
            , edit = InsertString "a" |> Just
          }
        , "cw"
        )
      )
    , ( "cawa"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit = InsertString "a" |> Just
          }
        , "caw"
        )
      )
    , ( "C"
      , ( { initialMode
            | modeName = ModeNameInsert
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
            | modeName = ModeNameInsert
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
      , ( { initialMode | modeName = ModeNameEx ":" }, ":" )
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
            , modeName = ModeNameEx ":"
          }
        , ":"
        )
      )
    , ( "/"
      , ( { initialMode | modeName = ModeNameEx "/" }, "/" )
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
            , modeName = ModeNameEx "/"
          }
        , "/"
        )
      )
    , ( "?"
      , ( { initialMode | modeName = ModeNameEx "?" }, "?" )
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
            , modeName = ModeNameEx "?"
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
            , modeName = ModeNameEx "?"
          }
        , "?"
        )
      )

    -- temp normal
    , ( "i<c-o>"
      , ( { initialMode | modeName = ModeNameTempNormal }, "i<c-o>" )
      )
    , ( "i<c-o><esc>"
      , ( { initialMode | modeName = ModeNameInsert }, "i" )
      )
    , ( "i<c-o>w"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
            , modeName = ModeNameInsert
          }
        , "i"
        )
      )
    , ( "i<c-o>i"
      , ( { initialMode | modeName = ModeNameInsert }, "i" )
      )
    , ( "i<c-o>c"
      , ( { initialMode | modeName = ModeNameTempNormal }
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
            , modeName = ModeNameInsert
          }
        , "i"
        )
      )
    , ( "cw<c-o>"
      , ( { initialMode | modeName = ModeNameTempNormal }, "cw<c-o>" )
      )
    , ( "cw<c-o><esc>"
      , ( { initialMode | modeName = ModeNameInsert }, "cw" )
      )
    , ( "cw<c-o>w"
      , ( { initialMode
            | edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
            , modeName = ModeNameInsert
          }
        , "cw"
        )
      )
    , ( "cw<c-o>i"
      , ( { initialMode | modeName = ModeNameInsert }, "cw" )
      )
    , ( "cw<c-o>c"
      , ( { initialMode | modeName = ModeNameTempNormal }
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
            , modeName = ModeNameInsert
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
            , modeName = ModeNameInsert
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
            , modeName = ModeNameInsert
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
            , modeName = ModeNameInsert
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
      , ( { initialMode | modeName = ModeNameVisual VisualName }, "v" )
      )
    , ( "V"
      , ( { initialMode | modeName = ModeNameVisual VisualNameLine }, "V" )
      )
    , ( "<c-v>"
      , ( { initialMode | modeName = ModeNameVisual VisualNameBlock }, "<c-v>" )
      )
    , ( "vV"
      , ( { initialMode | modeName = ModeNameVisual VisualNameLine }, "V" )
      )
    , ( "v<c-v>"
      , ( { initialMode | modeName = ModeNameVisual VisualNameBlock }, "<c-v>" )
      )
    , ( "Vv"
      , ( { initialMode | modeName = ModeNameVisual VisualName }, "v" )
      )
    , ( "V<c-v>"
      , ( { initialMode | modeName = ModeNameVisual VisualNameBlock }, "<c-v>" )
      )
    , ( "vv", ( initialMode, "" ) )
    , ( "VV", ( initialMode, "" ) )
    , ( "<c-v><c-v>", ( initialMode, "" ) )
    , ( "vw"
      , ( { initialMode
            | modeName = ModeNameVisual VisualName
            , edit =
                { direction = Forward, class = WordStart }
                    |> ByClass
                    |> Move
                    |> Just
          }
        , "v"
        )
      )
    , ( "vi", ( { initialMode | modeName = ModeNameVisual VisualName }, "vi" ) )
    , ( "viw"
      , ( { initialMode
            | modeName = ModeNameVisual VisualName
            , edit = Select Word False |> Just
          }
        , "v"
        )
      )
    , ( "v12iw"
      , ( { initialMode
            | modeName = ModeNameVisual VisualName
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
            | modeName = ModeNameInsert
            , edit = Delete VisualRange |> Just
          }
        , "vc"
        )
      )
    , ( "vcx"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit = InsertString "x" |> Just
          }
        , "vc"
        )
      )
    , ( "vC"
      , ( { initialMode
            | modeName = ModeNameInsert
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
