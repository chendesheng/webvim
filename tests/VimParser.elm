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
                motionOption ">]+="
                    |> Move BufferTop
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
                motionOption ">]+="
                    |> Move VLineDelta
                    |> Just
          }
        , ""
        )
      )
    , ( "gJ"
      , ( { initialMode
            | edit = Join False |> Just
            , recordKeys = "gJ"
          }
        , ""
        )
      )

    -- insert
    , ( "i"
      , ( { initialMode | modeName = ModeNameInsert }, "i" )
      )
    , ( "i<esc>"
      , ( { initialMode
            | modeName = ModeNameNormal
            , recordKeys = "i<inserts><esc>"
          }
        , ""
        )
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
                motionOption ">)$-"
                    |> MotionRange WordEdge
                    |> Delete
                    |> Just
          }
        , "cvw"
        )
      )
    , ( "cw<esc>"
      , ( { initialMode | recordKeys = "cw<inserts><esc>" }, "" )
      )
    , ( "cvw<esc>"
      , ( { initialMode | recordKeys = "cvw<inserts><esc>" }, "" )
      )
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
                motionOption ">)$-"
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
    , ( "c/"
      , ( { initialMode
            | modeName = ModeNameEx "/"
          }
        , "c/"
        )
      )
    , ( "c/<cr>"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit =
                Delete (MotionRange (MatchString LastSavedString) (motionOption ">)+-"))
                    |> Just
          }
        , "c/<cr>"
        )
      )
    , ( "c/<esc>", ( initialMode, "" ) )

    -- range operator
    , ( "d", ( initialMode, "d" ) )
    , ( "d<esc>", ( initialMode, "" ) )
    , ( "dw"
      , ( { initialMode
            | edit =
                motionOption ">)$-"
                    |> MotionRange WordStart
                    |> Delete
                    |> Just
            , recordKeys = "dw"
          }
        , ""
        )
      )
    , ( "dv", ( initialMode, "dv" ) )
    , ( "dvw"
      , ( { initialMode
            | edit =
                motionOption ">]$-"
                    |> MotionRange WordStart
                    |> Delete
                    |> Just
            , recordKeys = "dvw"
          }
        , ""
        )
      )
    , ( "dd"
      , ( { initialMode
            | edit =
                TextObject Line True
                    |> Delete
                    |> Just
            , recordKeys = "dd"
          }
        , ""
        )
      )
    , ( "\\<", ( initialMode, "\\<" ) )
    , ( "\\<<esc>", ( initialMode, "" ) )
    , ( "\\<w"
      , ( { initialMode
            | edit =
                motionOption ">)$-"
                    |> MotionRange WordStart
                    |> Indent False
                    |> Just
            , recordKeys = "\\<w"
          }
        , ""
        )
      )
    , ( "\\<v", ( initialMode, "\\<v" ) )
    , ( "\\<vw"
      , ( { initialMode
            | edit =
                motionOption ">]$-"
                    |> MotionRange WordStart
                    |> Indent False
                    |> Just
            , recordKeys = "\\<vw"
          }
        , ""
        )
      )
    , ( "J"
      , ( { initialMode
            | edit = Join True |> Just
            , recordKeys = "J"
          }
        , ""
        )
      )

    -- ex
    , ( ":"
      , ( { initialMode | modeName = ModeNameEx ":" }, ":" )
      )
    , ( ":<tab>"
      , ( { initialMode
            | modeName = ModeNameEx ":"
            , edit = SelectAutoComplete True |> Just
          }
        , ":"
        )
      )
    , ( ":<esc>"
      , ( initialMode, "" )
      )
    , ( ":<cr>"
      , ( { initialMode | edit = Execute |> Just }, "" )
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
                    |> Move (MatchString LastSavedString)
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
                    |> Move (MatchString LastSavedString)
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
    , ( "n"
      , ( { initialMode
            | edit =
                motionOption ">)+-"
                    |> Move (MatchString LastSavedString)
                    |> Just
          }
        , ""
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
            , edit = Put False |> Just
          }
        , "i"
        )
      )
    , ( "10i<c-o>8"
      , ( { initialMode
            | modeName = ModeNameTempNormal
            , count = Just 8
            , edit = Nothing
          }
        , "10i<c-o>8"
        )
      )
    , ( "10i<c-o>8j"
      , ( { initialMode
            | modeName = ModeNameInsert
            , count = Just 8
            , edit =
                motionOption ">]+="
                    |> Move LineDelta
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
    , ( "i<c-o>vd"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit = VisualRange False |> Delete |> Just
          }
        , "i"
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
            , edit = Put False |> Just
          }
        , "i"
        )
      )

    --count
    , ( "2"
      , ( { initialMode | count = Just 2 }, "2" )
      )
    , ( "2w"
      , ( { initialMode
            | count = Just 2
            , edit = Just (Move WordStart (motionOption ">)+-"))
          }
        , ""
        )
      )
    , ( "2x"
      , ( { initialMode
            | count = Just 2
            , edit = Just (Delete (MotionRange CharStart (motionOption ">)$-")))
            , recordKeys = "2x"
          }
        , ""
        )
      )
    , ( "23"
      , ( { initialMode | count = Just 23 }, "23" )
      )
    , ( "23<esc>", ( initialMode, "" ) )
    , ( "23\"a24"
      , ( { initialMode
            | register = "a"
            , count = Just (23 * 24)
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
      , ( { initialMode | modeName = ModeNameVisual VisualChars }, "v" )
      )
    , ( "V"
      , ( { initialMode | modeName = ModeNameVisual VisualLine }, "V" )
      )
    , ( "<c-v>"
      , ( { initialMode
            | modeName = ModeNameVisual VisualBlock
          }
        , "<c-v>"
        )
      )
    , ( "vV"
      , ( { initialMode | modeName = ModeNameVisual VisualLine }, "V" )
      )
    , ( "vo"
      , ( { initialMode
            | modeName = ModeNameVisual VisualChars
            , edit = Just VisualSwitchEnd
          }
        , "v"
        )
      )
    , ( "v<c-v>"
      , ( { initialMode
            | modeName = ModeNameVisual VisualBlock
          }
        , "<c-v>"
        )
      )
    , ( "Vv"
      , ( { initialMode | modeName = ModeNameVisual VisualChars }, "v" )
      )
    , ( "V<c-v>"
      , ( { initialMode | modeName = ModeNameVisual VisualBlock }, "<c-v>" )
      )
    , ( "vv", ( initialMode, "" ) )
    , ( "VV", ( initialMode, "" ) )
    , ( "<c-v><c-v>", ( initialMode, "" ) )
    , ( "vw"
      , ( { initialMode
            | modeName = ModeNameVisual VisualChars
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
            | modeName = ModeNameVisual VisualChars
          }
        , "vi"
        )
      )
    , ( "viw"
      , ( { initialMode
            | modeName = ModeNameVisual VisualChars
            , edit = Select Word False |> Just
          }
        , "v"
        )
      )
    , ( "v12iw"
      , ( { initialMode
            | modeName = ModeNameVisual VisualChars
            , count = Just 12
            , edit = Select Word False |> Just
          }
        , "v"
        )
      )
    , ( "vd"
      , ( { initialMode
            | edit = VisualRange False |> Delete |> Just
            , recordKeys = "v<visual>d"
          }
        , ""
        )
      )
    , ( "vaw"
      , ( { initialMode
            | edit = Select Word True |> Just
            , modeName = ModeNameVisual VisualChars
          }
        , "v"
        )
      )
    , ( "v10f"
      , ( { initialMode
            | count = Just 10
            , modeName = ModeNameVisual VisualChars
          }
        , "v10f"
        )
      )
    , ( "v10fm"
      , ( { initialMode
            | edit = Move (MatchChar "m" False) (motionOption ">]$-") |> Just
            , count = Just 10
            , modeName = ModeNameVisual VisualChars
          }
        , "v"
        )
      )
    , ( "vc"
      , ( { initialMode
            | modeName = ModeNameInsert
            , edit = VisualRange False |> Delete |> Just
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
                VisualRange True
                    |> Delete
                    |> Just
          }
        , "vC"
        )
      )
    , ( "v<visual>"
      , ( { initialMode
            | modeName = ModeNameVisual VisualChars
            , edit = Just RepeatLastVisual
          }
        , "v"
        )
      )
    , ( "vg"
      , ( { initialMode
            | modeName = ModeNameVisual VisualChars
          }
        , "vg"
        )
      )
    , ( "vgg"
      , ( { initialMode
            | modeName = ModeNameVisual VisualChars
            , edit = Just (Move BufferTop (motionOption ">]+="))
          }
        , "v"
        )
      )
    , ( "v/<esc>"
      , ( { initialMode
            | modeName = ModeNameVisual VisualChars
            , recordKeys = ""
          }
        , "v"
        )
      )
    , ( "r"
      , ( initialMode, "r" )
      )
    , ( "rb"
      , ( { initialMode
            | edit = Just (Replace "b")
            , recordKeys = "rb"
          }
        , ""
        )
      )

    -- text object
    , ( "diw"
      , ( { initialMode
            | edit = Just (Delete <| TextObject Word False)
            , recordKeys = "diw"
          }
        , ""
        )
      )
    , ( "di("
      , ( { initialMode
            | edit = Just (Delete <| TextObject (Pair '(') False)
            , recordKeys = "di("
          }
        , ""
        )
      )
    , ( "da}"
      , ( { initialMode
            | edit = Just (Delete <| TextObject (Pair '{') True)
            , recordKeys = "da}"
          }
        , ""
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
            (cases
             --|> List.filter (\( keys, _ ) -> keys == "v/<esc>")
            )
        )
