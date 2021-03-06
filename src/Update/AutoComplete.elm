module Update.AutoComplete exposing
    ( handleSelectHistory
    , handleSelectWord
    , startAutoCompleteFiles
    , startExAutoComplete
    , updateAutoCompleteEdit
    )

import Fs
import Helper.Fuzzy exposing (..)
import Helper.Helper
    exposing
        ( fileNameWordChars
        , getLast
        , pathBase
        , pathFileName
        , word
        )
import Internal.Position exposing (Position, positionShiftLeft)
import Internal.TextBuffer as B
import Menu as Mu
import Model.Buffer exposing (..)
import Model.Global exposing (..)
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
                (\( pos, s ) ->
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


filterAutoComplete : Bool -> Buffer -> Buffer
filterAutoComplete resetIfEmpty buf =
    updateAutoComplete
        (Maybe.andThen <|
            \({ pos, source, wordChars } as auto) ->
                let
                    target =
                        buf.lines
                            |> B.substring pos buf.view.cursor
                            |> B.toString
                in
                if
                    resetIfEmpty
                        && (target
                                |> String.filter (word wordChars)
                                |> String.isEmpty
                           )
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


updateAutoCompleteEdit : Global -> Buffer -> ( Buffer, Cmd Msg )
updateAutoCompleteEdit global buf =
    case buf.mode of
        Ex ({ exbuf } as newex) ->
            let
                s =
                    B.toString exbuf.lines

                trigger =
                    if isAutoCompleteStarted exbuf "$$%exHistory" then
                        "$$%exHistory"

                    else if String.startsWith ":o " s then
                        Fs.workingDir global.fs

                    else if String.startsWith ":b " s then
                        Fs.workingDir global.fs

                    else if String.startsWith ":cd " s || String.startsWith ":e " s then
                        s
                            |> String.trim
                            |> String.split " "
                            |> List.drop 1
                            |> getLast
                            |> Maybe.withDefault ""
                            |> Fs.absolutePath global.fs
                            |> pathBase (Fs.pathSeperator global.fs)

                    else
                        ""

                ( getList, clearAutoComplete ) =
                    if isAutoCompleteStarted exbuf "$$%exHistory" then
                        ( \_ _ -> Cmd.none, False )

                    else if String.startsWith ":o " s then
                        ( \fs _ -> sendListAllFiles fs, False )

                    else if String.startsWith ":b " s then
                        ( \_ _ ->
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
                        ( sendListDirs, False )

                    else
                        ( \_ _ -> Cmd.none, True )
            in
            if clearAutoComplete then
                ( setExbuf buf newex (updateAutoComplete (always Nothing) exbuf)
                , Cmd.none
                )

            else if isAutoCompleteStarted exbuf trigger then
                ( exbuf
                    |> filterAutoComplete False
                    |> setExbuf buf newex
                , Cmd.none
                )

            else
                ( buf, getList global.fs trigger )

        _ ->
            ( filterAutoComplete True buf, Cmd.none )


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
                    Fs.pathSeperator global.fs
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
                            (Fs.absolutePath global.fs path)
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
                            (pathFileName sep path)
                            exbuf

                    _ ->
                        exbuf
                )

        _ ->
            buf
