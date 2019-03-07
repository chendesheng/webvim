module Update.AutoComplete exposing
    ( autoCompleteTarget
    , filterAutoComplete
    , isAutoCompleteStarted
    , selectAutoComplete
    , startAutoComplete
    )

import Array as Array
import Helper.Fuzzy exposing (..)
import Helper.Helper exposing (rightChar, word)
import Internal.Position exposing (Position, positionShiftLeft)
import Internal.TextBuffer as B
import Menu as Mu
import Model exposing (AutoComplete, Buffer, Mode(..))
import Update.Buffer as Buf
import Update.Motion exposing (wordStringUnderCursor)


selectAutoComplete : Bool -> Buffer -> Buffer
selectAutoComplete forward buf =
    case buf.mode of
        Insert insert ->
            case insert.autoComplete of
                Just auto ->
                    let
                        { menu, pos, word } =
                            auto

                        menu1 =
                            if forward then
                                Mu.selectForward menu

                            else
                                Mu.selectBackward menu

                        txt =
                            Mu.getSelected menu1
                                |> Maybe.map .text
                                |> Maybe.withDefault word

                        ( y, x ) =
                            buf.view.cursor

                        buf1 =
                            Buf.transaction
                                [ B.Deletion pos buf.view.cursor
                                , B.Insertion pos (B.fromString txt)
                                ]
                                buf
                    in
                    { buf1
                        | mode =
                            Insert
                                { insert
                                    | autoComplete =
                                        Just
                                            { auto | menu = menu1 }
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
            buf.view.cursor
    in
    if x == 0 then
        Nothing

    else
        wordStringUnderCursor
            wordChars
            buf.lines
            ( y, x - 1 )
            |> Maybe.andThen
                (\(( pos, s ) as result) ->
                    if pos > ( y, x - 1 ) then
                        Nothing

                    else
                        let
                            ( _, x1 ) =
                                pos
                        in
                        Just ( pos, String.slice 0 (x - x1) s )
                )


isAutoCompleteStarted : Buffer -> String -> Bool
isAutoCompleteStarted buf newTrigger =
    case buf.mode of
        Insert { autoComplete } ->
            case autoComplete of
                Just { trigger } ->
                    trigger == newTrigger

                _ ->
                    False

        _ ->
            False


startAutoComplete :
    String
    -> String
    -> Int
    -> List String
    -> Position
    -> String
    -> Buffer
    -> Buffer
startAutoComplete wordChars trigger menuLeftOffset source pos word buf =
    case buf.mode of
        Insert insert ->
            { buf
                | mode =
                    Insert
                        { insert
                            | autoComplete =
                                Just
                                    { trigger = trigger
                                    , source = source
                                    , pos = pos
                                    , word = word
                                    , menu =
                                        word
                                            |> fuzzyMatch source
                                            |> Mu.init 10
                                    , wordChars = wordChars
                                    , menuLeftOffset = menuLeftOffset
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
                                |> B.substring pos buf.view.cursor
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
                                | word = target
                                , menu =
                                    target
                                        |> fuzzyMatch source
                                        |> Mu.init 10
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
                        Insert { insert | autoComplete = autoComplete }
                }

        _ ->
            buf
