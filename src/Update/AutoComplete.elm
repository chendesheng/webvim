module Update.AutoComplete exposing (..)

import Model exposing (Buffer, AutoComplete, Mode(..))
import Internal.TextBuffer as B
import Helper.Fuzzy exposing (..)
import Elm.Array as Array
import Update.Buffer as Buf
import Update.Motion exposing (wordStringUnderCursor)
import Internal.Position exposing (Position)


selectAutoComplete : Bool -> Buffer -> Buffer
selectAutoComplete forward buf =
    case buf.mode of
        Insert { autoComplete } ->
            case autoComplete of
                Just auto ->
                    let
                        { pos, matches, select } =
                            auto

                        newSelect =
                            (select
                                + if forward then
                                    1
                                  else
                                    -1
                            )
                                % Array.length matches

                        targetSelected =
                            newSelect == Array.length matches - 1

                        scrollTop =
                            if targetSelected then
                                auto.scrollTop
                            else if newSelect < auto.scrollTop then
                                newSelect
                            else if newSelect >= auto.scrollTop + 15 then
                                newSelect - 15 + 1
                            else
                                auto.scrollTop

                        txt =
                            matches
                                |> Array.get newSelect
                                |> Maybe.map .text
                                |> Maybe.withDefault ""

                        ( y, x ) =
                            buf.cursor

                        buf1 =
                            Buf.transaction
                                [ B.Deletion pos buf.cursor
                                , B.Insertion pos <|
                                    B.fromString txt
                                ]
                                buf
                    in
                        { buf1
                            | mode =
                                Insert
                                    { autoComplete =
                                        Just
                                            { auto
                                                | select = newSelect
                                                , scrollTop = scrollTop
                                            }
                                    }
                        }

                _ ->
                    buf

        _ ->
            buf


autoCompleteTarget : Buffer -> Maybe ( Position, String )
autoCompleteTarget buf =
    let
        ( y, x ) =
            buf.cursor
    in
        wordStringUnderCursor
            buf.config.wordChars
            buf.lines
            ( y, max 0 (x - 1) )


isAutoCompleteStarted : Buffer -> Bool
isAutoCompleteStarted buf =
    case buf.mode of
        Insert { autoComplete } ->
            autoComplete /= Nothing

        _ ->
            False


startAutoComplete : List String -> Position -> String -> Buffer -> Buffer
startAutoComplete source pos word buf =
    case buf.mode of
        Insert _ ->
            { buf
                | mode =
                    Insert
                        { autoComplete =
                            Just
                                { source = source
                                , pos = pos
                                , matches =
                                    word
                                        |> fuzzyMatch source
                                        |> Array.fromList
                                        |> Array.push
                                            { text = word
                                            , matches = []
                                            }
                                , select = -1
                                , scrollTop = 0
                                }
                        }
            }

        _ ->
            buf


filterAutoComplete : Buffer -> Buffer
filterAutoComplete buf =
    case buf.mode of
        Insert { autoComplete } ->
            { buf
                | mode =
                    Insert
                        { autoComplete =
                            case autoComplete of
                                Just auto ->
                                    let
                                        { pos, source } =
                                            auto

                                        target =
                                            buf.lines
                                                |> B.substring
                                                    pos
                                                    buf.cursor
                                                |> B.toString
                                                |> String.trim

                                        matches =
                                            if String.length target > 0 then
                                                fuzzyMatch source target
                                            else
                                                []
                                    in
                                        if List.isEmpty matches then
                                            Nothing
                                        else
                                            Just
                                                { auto
                                                    | matches =
                                                        Array.push
                                                            { text = target
                                                            , matches = []
                                                            }
                                                            (Array.fromList
                                                                matches
                                                            )
                                                }

                                _ ->
                                    Nothing
                        }
            }

        _ ->
            buf
