module Update.AutoComplete exposing
    ( filterAutoComplete
    , handleSelectHistory
    , handleSelectWord
    , startAutoCompleteFiles
    , startExAutoComplete
    , updateAutoCompleteExEdit
    )

import Array as Array
import Helper.Fuzzy exposing (..)
import Helper.Helper
    exposing
        ( fileNameWordChars
        , getLast
        , pathBase
        , rightChar
        , toAbsolutePath
        , word
        )
import Internal.Position exposing (Position, positionShiftLeft)
import Internal.TextBuffer as B
import Menu as Mu
import Model
    exposing
        ( AutoComplete
        , Buffer
        , ExMode
        , Global
        , Mode(..)
        , getBuffers
        , setExbuf
        )
import Task
import Update.Buffer as Buf
import Update.Message exposing (Msg(..))
import Update.Motion exposing (wordStringUnderCursor)
import Update.Service exposing (..)


updateAutoComplete :
    (Maybe AutoComplete -> Maybe AutoComplete)
    -> Buffer
    -> Buffer
updateAutoComplete fn buf =
    case buf.mode of
        Insert insert ->
            { buf
                | mode =
                    Insert { insert | autoComplete = fn insert.autoComplete }
            }

        _ ->
            buf


withAutoComplete : (AutoComplete -> a) -> Buffer -> Maybe a
withAutoComplete fn buf =
    case buf.mode of
        Insert { autoComplete } ->
            Maybe.map fn autoComplete

        _ ->
            Nothing


selectAutoComplete : Bool -> Buffer -> Buffer
selectAutoComplete forward buf =
    buf
        |> withAutoComplete
            (\{ pos, word, menu } ->
                let
                    menu1 =
                        if forward then
                            Mu.selectForward menu

                        else
                            Mu.selectBackward menu

                    txt =
                        Mu.getSelected menu1
                            |> Maybe.map .text
                            |> Maybe.withDefault word
                            |> B.fromString
                in
                buf
                    |> Buf.transaction
                        [ B.Deletion pos buf.view.cursor
                        , B.Insertion pos txt
                        ]
                    |> updateAutoComplete
                        (Maybe.map (\auto -> { auto | menu = menu1 }))
            )
        |> Maybe.withDefault buf


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
    buf
        |> withAutoComplete .trigger
        |> Maybe.map ((==) newTrigger)
        |> Maybe.withDefault False


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
    updateAutoComplete
        (always <|
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
        )
        buf


filterAutoComplete : Buffer -> Buffer
filterAutoComplete buf =
    updateAutoComplete
        (Maybe.andThen
            (\({ pos, source, wordChars } as auto) ->
                let
                    target =
                        buf.lines
                            |> B.substring pos buf.view.cursor
                            |> B.toString
                in
                if
                    target
                        |> String.filter (word wordChars)
                        |> String.isEmpty
                then
                    Nothing

                else
                    Just
                        { auto
                            | word = target
                            , menu =
                                target
                                    |> fuzzyMatch source
                                    |> Mu.init 10
                        }
            )
        )
        buf



-- UPDATE: Buffer -> Buffer


handleSelectWord : Bool -> Buffer -> Buffer
handleSelectWord forward buf =
    case buf.mode of
        Ex ex ->
            ex.exbuf
                |> selectAutoComplete forward
                |> setExbuf buf ex

        Insert { autoComplete } ->
            case autoComplete of
                Just _ ->
                    selectAutoComplete forward buf

                _ ->
                    case autoCompleteTarget buf.config.wordChars buf of
                        Just ( pos, word ) ->
                            let
                                exclude =
                                    wordStringUnderCursor
                                        buf.config.wordChars
                                        buf.lines
                                        (positionShiftLeft buf.view.cursor)
                                        |> Maybe.map Tuple.second
                                        |> Maybe.withDefault ""

                                words =
                                    Buf.toWords exclude buf
                            in
                            if List.isEmpty words then
                                buf

                            else
                                buf
                                    |> startAutoComplete
                                        buf.config.wordChars
                                        ""
                                        0
                                        words
                                        pos
                                        word
                                    |> selectAutoComplete forward

                        _ ->
                            buf

        _ ->
            buf


handleSelectHistory : Bool -> List String -> Buffer -> Buffer
handleSelectHistory forward exHistory buf =
    case buf.mode of
        Ex ex ->
            if isAutoCompleteStarted ex.exbuf "$$%exHistory" then
                ex.exbuf
                    |> selectAutoComplete forward
                    |> setExbuf buf ex

            else
                ex.exbuf
                    |> startAutoComplete ""
                        "$$%exHistory"
                        1
                        (List.reverse exHistory)
                        ( 0, 1 )
                        (B.toString ex.exbuf.lines
                            |> String.dropLeft 1
                        )
                    |> selectAutoComplete forward
                    |> setExbuf buf ex

        _ ->
            buf


clearExBufAutoComplete : Buffer -> Buffer
clearExBufAutoComplete exbuf =
    { exbuf
        | mode =
            case exbuf.mode of
                Insert insert ->
                    Insert
                        { insert
                            | autoComplete =
                                Nothing
                        }

                _ ->
                    exbuf.mode
    }


updateAutoCompleteExEdit : Global -> Buffer -> ( Buffer, Cmd Msg )
updateAutoCompleteExEdit global buf =
    case buf.mode of
        Ex ({ exbuf } as newex) ->
            let
                s =
                    B.toString exbuf.lines

                trigger =
                    if isAutoCompleteStarted exbuf "$$%exHistory" then
                        "$$%exHistory"

                    else if String.startsWith ":o " s then
                        global.cwd

                    else if String.startsWith ":b " s then
                        global.cwd

                    else
                        s
                            |> String.trim
                            |> String.split " "
                            |> getLast
                            |> Maybe.withDefault ""
                            |> toAbsolutePath
                                global.pathSeperator
                                global.homedir
                                global.cwd

                ( getList, clearAutoComplete ) =
                    if isAutoCompleteStarted exbuf "$$%exHistory" then
                        ( \a b c -> Cmd.none, False )

                    else if String.startsWith ":o " s then
                        ( sendListAllFiles, False )

                    else if String.startsWith ":b " s then
                        ( \a b c ->
                            Task.succeed
                                (global.buffers
                                    |> getBuffers
                                    |> List.filterMap
                                        (\{ path } ->
                                            if path /= buf.path && path /= "" then
                                                Just path

                                            else
                                                Nothing
                                        )
                                    |> List.map (Buf.shortPath global)
                                )
                                |> Task.perform ListBuffers
                        , False
                        )

                    else if String.startsWith ":e " s then
                        ( sendListFiles, False )

                    else if String.startsWith ":cd " s then
                        ( sendListDirectories, False )

                    else
                        ( \a b c -> Cmd.none, True )
            in
            if clearAutoComplete then
                ( setExbuf buf newex (clearExBufAutoComplete exbuf)
                , Cmd.none
                )

            else if isAutoCompleteStarted exbuf trigger then
                ( exbuf
                    |> filterAutoComplete
                    |> setExbuf buf newex
                , Cmd.none
                )

            else
                ( buf
                , getList
                    global.service
                    global.pathSeperator
                    trigger
                )

        _ ->
            ( buf, Cmd.none )


startExAutoComplete : Int -> String -> List String -> Buffer -> Buffer
startExAutoComplete offset trigger candidates buf =
    case buf.mode of
        Ex ({ exbuf } as ex) ->
            let
                pos =
                    ( 0, offset + 1 )

                target =
                    exbuf.lines
                        |> B.substring pos exbuf.view.cursor
                        |> B.toString
            in
            setExbuf buf
                ex
                (startAutoComplete
                    fileNameWordChars
                    trigger
                    offset
                    candidates
                    pos
                    target
                    exbuf
                )

        _ ->
            buf


startAutoCompleteFiles : List String -> Global -> Buffer -> Buffer
startAutoCompleteFiles files global buf =
    case buf.mode of
        Ex ({ exbuf } as ex) ->
            let
                sep =
                    global.pathSeperator
            in
            setExbuf buf
                ex
                (case
                    exbuf.lines
                        |> B.toString
                        |> String.split " "
                 of
                    [ prefix, path ] ->
                        startAutoComplete
                            fileNameWordChars
                            (toAbsolutePath sep global.homedir global.cwd path)
                            (String.length prefix)
                            files
                            ( 0
                            , (path
                                |> pathBase sep
                                |> String.length
                              )
                                + String.length prefix
                                + String.length sep
                            )
                            ""
                            exbuf

                    _ ->
                        exbuf
                )

        _ ->
            buf
