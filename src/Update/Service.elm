module Update.Service exposing
    ( bootDecoder
    , lineDiffDecoder
    , parseFileList
    , sendCd
    , sendListAllFiles
    , sendListDirectories
    , sendListFiles
    , sendMkDir
    , sendReadBuffer
    , sendReadClipboard
    , sendSearch
    , sendWriteBuffer
    , sendWriteClipboard
    )

import Array as Array
import Dict exposing (Dict)
import Helper.Helper exposing (..)
import Http
import Internal.Syntax exposing (TokenType(..))
import Internal.TextBuffer as B exposing (Patch(..), lineBreak)
import Internal.Window as Win
import Json.Decode as Decode exposing (decodeString)
import List
import Model exposing (Key, ServerArgs)
import Model.Buffer exposing (..)
import Model.BufferHistory exposing (emptyBufferHistory)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Task exposing (Task)
import Update.Message exposing (Msg(..), TokenizeResponse(..))
import Update.Tokenize exposing (sendTokenizeTask)
import Vim.AST exposing (AST)


lineDiffDecoder : Decode.Decoder (List Patch)
lineDiffDecoder =
    (Decode.field "type" Decode.string
        |> Decode.andThen
            (\tipe ->
                case tipe of
                    "+" ->
                        Decode.map2
                            Insertion
                            (Decode.field "from" Decode.int
                                |> Decode.map (\y -> ( y, 0 ))
                            )
                            (Decode.field "value" Decode.string
                                |> Decode.map B.fromString
                            )

                    "-" ->
                        Decode.map2
                            Deletion
                            (Decode.field "from" Decode.int
                                |> Decode.map (\y -> ( y, 0 ))
                            )
                            (Decode.field "to" Decode.int
                                |> Decode.map (\y -> ( y, 0 ))
                            )

                    _ ->
                        Decode.fail "invalid change type"
            )
    )
        |> Decode.list


sendReadBuffer : String -> Int -> Win.Path -> Buffer -> Cmd Msg
sendReadBuffer url viewHeight framePath buf =
    { method = "GET"
    , headers = []
    , url = url ++ "/read?path=" ++ buf.path
    , body = Http.emptyBody
    , resolver = Http.stringResolver httpStringResponse
    , timeout = Nothing
    }
        |> Http.task
        |> Task.andThen
            (\( headers, body ) ->
                let
                    s =
                        if String.endsWith lineBreak body then
                            body

                        else
                            body ++ lineBreak

                    lines =
                        B.fromStringExpandTabs buf.config.tabSize 0 s

                    lastModified =
                        Dict.get "last-modified" headers
                            |> Maybe.withDefault ""

                    tokenizeLines =
                        Tuple.first buf.view.cursor + viewHeight * 2

                    response =
                        { syntaxEnabled = True
                        , lines = lines
                        , syntax = Array.empty
                        , lastModified = lastModified
                        }
                in
                if buf.config.syntax then
                    sendTokenizeTask url
                        { bufId = buf.id
                        , path = buf.path
                        , version = 0
                        , line = 0
                        , lines =
                            lines
                                |> B.sliceLines 0 tokenizeLines
                                |> B.toString
                        }
                        |> Task.map
                            (\res ->
                                case res of
                                    TokenizeSuccess _ syntax ->
                                        { response | syntax = syntax }

                                    TokenizeError err ->
                                        if err == "noextension" then
                                            { response | syntaxEnabled = False }

                                        else
                                            response

                                    _ ->
                                        response
                            )
                        |> Task.onError (\_ -> Task.succeed response)

                else
                    Task.succeed { response | syntaxEnabled = False }
            )
        |> Task.attempt
            (\result ->
                case
                    result
                        |> Result.map
                            (\{ syntax, syntaxEnabled, lines, lastModified } ->
                                let
                                    history =
                                        buf.history

                                    config =
                                        buf.config
                                in
                                { buf
                                    | lines = lines
                                    , config = { config | syntax = syntaxEnabled }
                                    , syntax = syntax
                                    , syntaxDirtyFrom = Array.length syntax
                                    , history =
                                        if history.lastModified == lastModified then
                                            history

                                        else
                                            { emptyBufferHistory | lastModified = lastModified }
                                }
                            )
                of
                    Ok b ->
                        Read (Ok ( framePath, b ))

                    Err ((Http.BadStatus code) as err) ->
                        case code of
                            -- 404 is ok here, the buffer is newly created
                            404 ->
                                Read (Ok ( framePath, buf ))

                            _ ->
                                Read (Err err)

                    Err err ->
                        Read (Err err)
            )


sendWriteBuffer : String -> String -> Buffer -> Cmd Msg
sendWriteBuffer url path buf =
    Http.post
        { url = url ++ "/write?path=" ++ path
        , body = Http.stringBody "text/plain" <| B.toString buf.lines
        , expect =
            Http.expectStringResponse
                Write
                (httpJsonResponse lineDiffDecoder
                    >> Result.map
                        (\( headers, patches ) ->
                            ( Dict.get "last-modified" headers |> Maybe.withDefault ""
                            , patches
                            )
                        )
                    >> Result.mapError httpErrorMessage
                )
        }


parseFileList : String -> Result Http.Error String -> Result String (List String)
parseFileList sep resp =
    case resp of
        Ok s ->
            s
                |> String.split "\n"
                |> List.map (normalizePath sep)
                |> Ok

        Err err ->
            Err <| httpErrorMessage err


sendListAllFiles : String -> String -> String -> Cmd Msg
sendListAllFiles url sep cwd =
    Http.get
        { url = url ++ "/ls?cwd=" ++ cwd
        , expect = Http.expectString (parseFileList sep >> ListAllFiles)
        }


sendListFiles : String -> String -> String -> Cmd Msg
sendListFiles url sep cwd =
    let
        trimSep s =
            if String.toLower s == ".ds_store" then
                Nothing

            else
                Just <| s
    in
    Http.get
        { url = url ++ "/ld?cwd=" ++ cwd
        , expect =
            Http.expectString
                (parseFileList sep
                    >> Result.map (List.filterMap trimSep)
                    >> ListFiles
                )
        }


sendListDirectories : String -> String -> String -> Cmd Msg
sendListDirectories url sep cwd =
    let
        trimSep s =
            if s == "./" || s == "../" then
                Nothing

            else if String.endsWith sep s then
                Just <| String.dropRight (String.length sep) s

            else
                Nothing
    in
    Http.get
        { url = url ++ "/ld?cwd=" ++ cwd
        , expect =
            Http.expectString
                (parseFileList sep
                    >> Result.map (List.filterMap trimSep)
                    >> ListDirectries
                )
        }


sendReadClipboard : Bool -> Key -> String -> AST -> Cmd Msg
sendReadClipboard replaying key url op =
    Http.get
        { url = url ++ "/clipboard"
        , expect =
            Http.expectString <|
                Result.map
                    (\s ->
                        { replaying = replaying
                        , key = key
                        , ast = op
                        , s = s
                        }
                    )
                    >> ReadClipboard
        }


sendWriteClipboard : String -> String -> Cmd Msg
sendWriteClipboard url str =
    Http.post
        { url = url ++ "/clipboard"
        , body = Http.stringBody "text/plain" str
        , expect =
            Http.expectJson
                (\result ->
                    WriteClipboard <|
                        Result.mapError httpErrorMessage result
                )
                (Decode.succeed ())
        }


sendSearch : String -> String -> String -> Cmd Msg
sendSearch url cwd s =
    Http.get
        { url = url ++ "/search?s=" ++ s ++ "&cwd=" ++ cwd
        , expect =
            Http.expectString
                (\res ->
                    SearchResult <|
                        Result.mapError httpErrorMessage res
                )
        }


sendCd : String -> String -> Cmd Msg
sendCd url cwd =
    Http.get
        { url = url ++ "/cd?cwd=" ++ cwd
        , expect =
            Http.expectString
                (\res ->
                    SetCwd <|
                        Result.mapError httpErrorMessage res
                )
        }


sendMkDir : String -> String -> Cmd Msg
sendMkDir url path =
    Http.get
        { url = url ++ "/mkdir?path=" ++ path
        , expect =
            Http.expectString
                (Result.mapError httpErrorMessage
                    >> Result.map (always ())
                    >> MakeDir
                )
        }


bootDecoder : Decode.Decoder ServerArgs
bootDecoder =
    Decode.map2
        (\homedir pathSeperator ->
            { pathSeperator = pathSeperator
            , homedir = homedir
            }
        )
        (Decode.field "homedir" Decode.string)
        (Decode.field "pathSeperator" Decode.string)
