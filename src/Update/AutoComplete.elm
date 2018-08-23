module Update.AutoComplete exposing (..)

import Model exposing (Buffer, AutoComplete, Mode(..))
import Internal.TextBuffer as B
import Helper.Fuzzy exposing (..)
import Array as Array
import Update.Buffer as Buf
import Update.Motion exposing (wordStringUnderCursor)
import Internal.Position exposing (Position)
import Helper.Helper exposing (word, rightChar)


selectAutoComplete : Bool -> Buffer -> Buffer
selectAutoComplete forward buf =
    case buf.mode of
        Insert insert ->
            case insert.autoComplete of
                Just auto ->
                    let
                        { pos, matches, select } =
                            auto

                        newSelect =
                            modBy
                                (Array.length matches)
                                (select
                                    + if forward then
                                        1
                                      else
                                        -1
                                )

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
                                    { insert
                                        | autoComplete =
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


autoCompleteTarget : String -> Buffer -> Maybe ( Position, String )
autoCompleteTarget wordChars buf =
    let
        ( y, x ) =
            buf.cursor
    in
        wordStringUnderCursor
            wordChars
            buf.lines
            ( y, max 0 (x - 1) )
            |> Maybe.andThen
                (\(( pos, s ) as result) ->
                    if pos >= buf.cursor then
                        Nothing
                    else
                        Just result
                )


isAutoCompleteStarted : Buffer -> Bool
isAutoCompleteStarted buf =
    case buf.mode of
        Insert { autoComplete } ->
            autoComplete /= Nothing

        _ ->
            False


startAutoComplete :
    String
    -> List String
    -> Position
    -> String
    -> Buffer
    -> Buffer
startAutoComplete wordChars source pos word buf =
    case buf.mode of
        Insert insert ->
            { buf
                | mode =
                    Insert
                        { insert
                            | autoComplete =
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
                                    , wordChars = wordChars
                                    }
                        }
            }

        _ ->
            buf


filterAutoComplete : Buffer -> Buffer
filterAutoComplete buf =
    let
        updateAutoComplete autoComplete =
            case autoComplete of
                Just auto ->
                    let
                        { pos, source, wordChars } =
                            auto

                        target =
                            buf.lines
                                |> B.substring pos buf.cursor
                                |> B.toString
                    in
                        if
                            target
                                |> rightChar
                                |> Maybe.map (word wordChars)
                                |> Maybe.withDefault True
                        then
                            Just
                                { auto
                                    | matches =
                                        Array.push
                                            { text = target
                                            , matches = []
                                            }
                                            (target
                                                |> fuzzyMatch source
                                                |> Array.fromList
                                            )
                                }
                        else
                            Nothing

                _ ->
                    Nothing
    in
        case buf.mode of
            Insert insert ->
                let
                    autoComplete =
                        updateAutoComplete insert.autoComplete
                in
                    -- avoid create new mode object when autoComplete hide
                    if
                        (insert.autoComplete == Nothing)
                            && (autoComplete == Nothing)
                    then
                        buf
                    else
                        { buf
                            | mode =
                                Insert
                                    { insert
                                        | autoComplete =
                                            updateAutoComplete autoComplete
                                    }
                        }

            _ ->
                buf
