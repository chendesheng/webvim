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
                motionOption ">)+-"
                    |> Move WordStart
                    |> Just
          }
        , ""
        )
      )
    , ( "b"
      , ( { initialMode
            | edit =
                motionOption "<)+-"
                    |> Move WordStart
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
            | edit =
                motionOption ">)+="
                    |> Move (LineNumber 0)
                    |> Just
          }
        , ""
        )
      )
    , ( "g<esc>"
      , ( initialMode, "" )
      )
    , ( "gj"
      , ( { initialMode
            | edit =
                motionOption ">)+="
                    |> Move (VLineDelta 1)
                    |> Just
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
            , edit = InsertString (TextLiteral "a") |> Just
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
                motionOption ">]$-"
                    |> MotionRange WordEdge
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
                motionOption ">]+-"
                    |> MotionRange WordStart
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
            , edit = InsertString (TextLiteral "a") |> Just
          }
        , "cw"
        )
      )
    , ( "cawa"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit = InsertString (TextLiteral "a") |> Just
          }
        , "caw"
        )
      )
    , ( "C"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit =
                motionOption ">]$-"
                    |> MotionRange LineEnd
                    |> Delete
                    |> Just
          }
        , "C"
        )
      )
    , ( "Cw"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit = InsertString (TextLiteral "w") |> Just
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
                motionOption ">)+-"
                    |> MotionRange WordStart
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
                motionOption ">]+-"
                    |> MotionRange WordStart
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
                motionOption ">)+-"
                    |> MotionRange WordStart
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
                motionOption ">]+-"
                    |> MotionRange WordStart
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
            | edit = InsertString (TextLiteral "a") |> Just
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
      , ( { initialMode
            | edit =
                motionOption ">)+-"
                    |> Move MatchString
                    |> Just
          }
        , ""
        )
      )
    , ( "/a"
      , ( { initialMode
            | edit = InsertString (TextLiteral "a") |> Just
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
      , ( { initialMode
            | edit =
                motionOption "<)+-"
                    |> Move MatchString
                    |> Just
          }
        , ""
        )
      )
    , ( "?a"
      , ( { initialMode
            | edit = InsertString (TextLiteral "a") |> Just
            , modeName = ModeNameEx "?"
          }
        , "?"
        )
      )
    , ( "?<c-h>"
      , ( { initialMode
            | edit =
                motionOption "<)+-"
                    |> MotionRange CharStart
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
                motionOption ">)+-"
                    |> Move WordStart
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
                motionOption ">]+-"
                    |> MotionRange WordEnd
                    |> Delete
                    |> Just
            , modeName = ModeNameInsert
          }
        , "i"
        )
      )
    , ( "i<c-o>u"
      , ( { initialMode
            | edit =
                InsertString (TextLiteral "u")
                    |> Just
            , modeName = ModeNameInsert
          }
        , "i"
        )
      )
    , ( "i<c-o><c-r>"
      , ( { initialMode | modeName = ModeNameInsert }
        , "i<c-r>"
        )
      )
    , ( "i<c-o><c-r>a"
      , ( { initialMode
            | register = "a"
            , modeName = ModeNameInsert
            , edit = Put Forward |> Just
          }
        , "i"
        )
      )
    , ( "10i<c-o>8"
      , ( { initialMode
            | modeName = ModeNameTempNormal
            , count = 8
            , edit = Nothing
          }
        , "10i<c-o>8"
        )
      )
    , ( "10i<c-o>8j"
      , ( { initialMode
            | modeName = ModeNameInsert
            , count = 8
            , edit =
                motionOption ">]+="
                    |> Move (LineDelta 1)
                    |> Just
          }
        , "10i"
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
                motionOption ">)+-"
                    |> Move WordStart
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
                motionOption ">]+-"
                    |> MotionRange WordEnd
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
                motionOption ">)+-"
                    |> Move WordStart
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
                motionOption ">]$-"
                    |> MotionRange WordEdge
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
            , edit = InsertString (TextLiteral "q") |> Just
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
                motionOption ">)+-"
                    |> Move WordStart
                    |> Just
          }
        , ""
        )
      )
    , ( "i<c-r>"
      , ( { initialMode | modeName = ModeNameInsert }
        , "i<c-r>"
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
                motionOption ">)+-"
                    |> Move WordStart
                    |> Just
          }
        , "v"
        )
      )
    , ( "vi"
      , ( { initialMode
            | modeName = ModeNameVisual VisualName
          }
        , "vi"
        )
      )
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
            , edit = InsertString (TextLiteral "x") |> Just
          }
        , "vc"
        )
      )
    , ( "vC"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit =
                motionOption ">]$-"
                    |> MotionRange LineEnd
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
