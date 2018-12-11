module TestMotion exposing (cleanLineBreaks, suite)

import Expect exposing (Expectation)
import Helper.Helper exposing (regex)
import Internal.TextBuffer as B
import Regex as Re
import Test exposing (..)
import Update.Motion exposing (..)


cleanLineBreaks : String -> String
cleanLineBreaks s =
    Re.replace (regex "[\n]+") (always "\n") s


suite : Test
suite =
    describe "Motion" <|
        [ describe "wordUnderCursor" <|
            let
                testWordUnderCursor lines result =
                    test
                        (lines
                            -- first two characters are
                            -- for padding so when error message
                            -- display in console more nice
                            |> List.map ((++) "  ")
                            |> String.join "\n"
                            |> cleanLineBreaks
                        )
                        (\_ ->
                            case lines of
                                [ line, cursor ] ->
                                    case
                                        cursor
                                            |> String.indexes "^"
                                            |> List.head
                                    of
                                        Just x ->
                                            Expect.equal
                                                (wordStringUnderCursor ""
                                                    (B.fromString line)
                                                    ( 0, x )
                                                )
                                                result

                                        _ ->
                                            Expect.fail "no cursor in test case"

                                _ ->
                                    Expect.fail "test case format error"
                        )
            in
            [ testWordUnderCursor
                [ "aaa \n"
                , " ^    "
                ]
                (Just ( ( 0, 0 ), "aaa" ))
            , testWordUnderCursor
                [ "aaa bbb\n"
                , "   ^    "
                ]
                (Just ( ( 0, 4 ), "bbb" ))
            , testWordUnderCursor
                [ "aaa \n"
                , "   ^   "
                ]
                Nothing
            , testWordUnderCursor
                [ " \n"
                , "^"
                ]
                Nothing
            ]
        ]
