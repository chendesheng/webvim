module Update.Service exposing
    ( bootDecoder
    , getBodyAndHeaders
    , lineDiffDecoder
    , parseFileList
    , post
    , postRespHeaders
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
import Bitwise as Bit
import Char
import Dict exposing (Dict)
import Helper.Helper exposing (..)
import Http
import Internal.Jumps exposing (Location)
import Internal.Position
    exposing
        ( Position
        , endPositionDecoder
        , positionDecoder
        , regionDecoder
        )
import Internal.Syntax exposing (Token, TokenType(..))
import Internal.TextBuffer as B exposing (Patch(..), lineBreak)
import Internal.Window as Win
import Json.Decode as Decode exposing (decodeString)
import List
import Model exposing (Flags, Key, ServerArgs)
import Model.Buffer exposing (..)
import Model.BufferHistory exposing (emptyBufferHistory)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Regex as Re
import Task exposing (Task)
import Update.Message
    exposing
        ( BufferIdentifier
        , Msg(..)
        , TokenizeRequest
        , TokenizeResponse(..)
        )
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


getBodyAndHeaders : String -> Http.Request ( Dict String String, String )
getBodyAndHeaders url =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\response ->
                    Ok ( response.headers, response.body )
                )
        , timeout = Nothing
        , withCredentials = False
        }


sendReadBuffer : String -> Int -> Win.Path -> Buffer -> Cmd Msg
sendReadBuffer url viewHeight framePath buf =
    url
        ++ "/read?path="
        ++ buf.path
        |> getBodyAndHeaders
        |> Http.toTask
        |> Task.andThen
            (\( headers, s_ ) ->
                let
                    s =
                        if String.endsWith lineBreak s_ then
                            s_

                        else
                            s_ ++ lineBreak

                    lines =
                        B.fromStringExpandTabs buf.config.tabSize 0 s

                    lastModified =
                        Dict.get "last-modified" headers
                            |> Maybe.withDefault ""

                    tokenizeLines =
                        Tuple.first buf.view.cursor + viewHeight * 2
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
                                        { syntaxEnabled = True
                                        , lines = lines
                                        , syntax = syntax
                                        , lastModified = lastModified
                                        }

                                    TokenizeError err ->
                                        if err == "noextension" then
                                            { syntaxEnabled = False
                                            , lines = lines
                                            , syntax = Array.empty
                                            , lastModified = lastModified
                                            }

                                        else
                                            { syntaxEnabled = True
                                            , lines = lines
                                            , syntax = Array.empty
                                            , lastModified = lastModified
                                            }

                                    _ ->
                                        { syntaxEnabled = True
                                        , lines = lines
                                        , syntax = Array.empty
                                        , lastModified = lastModified
                                        }
                            )
                        |> Task.onError
                            (\_ ->
                                Task.succeed
                                    { syntaxEnabled = True
                                    , lines = lines
                                    , syntax = Array.empty
                                    , lastModified = lastModified
                                    }
                            )

                else
                    Task.succeed
                        { syntaxEnabled = False
                        , lines = lines
                        , syntax = Array.empty
                        , lastModified = lastModified
                        }
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

                    Err ((Http.BadStatus resp) as err) ->
                        case resp.status.code of
                            -- 404 is ok here, the buffer is newly created
                            404 ->
                                Read (Ok ( framePath, buf ))

                            _ ->
                                Read (Err err)

                    Err err ->
                        Read (Err err)
            )


post : String -> Http.Body -> Http.Request String
post url body =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


postRespHeaders :
    String
    -> Http.Body
    -> Decode.Decoder a
    -> Http.Request ( Dict String String, a )
postRespHeaders url inputBody decoder =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = inputBody
        , expect =
            Http.expectStringResponse <|
                \{ headers, body } ->
                    case Decode.decodeString decoder body of
                        Err decodeError ->
                            Err (Decode.errorToString decodeError)

                        Ok value ->
                            Ok ( headers, value )
        , timeout = Nothing
        , withCredentials = False
        }


sendWriteBuffer : String -> String -> Buffer -> Cmd Msg
sendWriteBuffer url path buf =
    lineDiffDecoder
        |> postRespHeaders
            (url ++ "/write?path=" ++ path)
            (Http.stringBody "text/plain" <| B.toString buf.lines)
        |> Http.send
            (\res ->
                case res of
                    Ok ( headers, patches ) ->
                        Write <|
                            Ok ( Dict.get "last-modified" headers |> Maybe.withDefault "", patches )

                    Err s ->
                        Write <| Err <| httpErrorMessage s
            )


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
    Http.getString (url ++ "/ls?cwd=" ++ cwd)
        |> Http.send (parseFileList sep >> ListAllFiles)


sendListFiles : String -> String -> String -> Cmd Msg
sendListFiles url sep cwd =
    let
        trimSep s =
            if String.toLower s == ".ds_store" then
                Nothing

            else
                Just <| s
    in
    Http.getString (url ++ "/ld?cwd=" ++ cwd)
        |> Http.send
            (parseFileList sep
                >> Result.map (List.filterMap trimSep)
                >> ListFiles
            )


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
    Http.getString (url ++ "/ld?cwd=" ++ cwd)
        |> Http.send
            (parseFileList sep
                >> Result.map (List.filterMap trimSep)
                >> ListDirectries
            )


sendReadClipboard : Bool -> Key -> String -> AST -> Cmd Msg
sendReadClipboard replaying key url op =
    Http.getString (url ++ "/clipboard")
        |> Http.send
            (Result.map
                (\s ->
                    { replaying = replaying
                    , key = key
                    , ast = op
                    , s = s
                    }
                )
                >> ReadClipboard
            )


sendWriteClipboard : String -> String -> Cmd Msg
sendWriteClipboard url str =
    Http.request
        { method = "POST"
        , headers = []
        , url = url ++ "/clipboard"
        , body = Http.stringBody "text/plain" str
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send
            (\result ->
                WriteClipboard <|
                    Result.mapError httpErrorMessage result
            )


sendSearch : String -> String -> String -> Cmd Msg
sendSearch url cwd s =
    Http.getString (url ++ "/search?s=" ++ s ++ "&cwd=" ++ cwd)
        |> Http.send
            (\res ->
                SearchResult <|
                    Result.mapError httpErrorMessage res
            )


sendCd : String -> String -> Cmd Msg
sendCd url cwd =
    Http.getString (url ++ "/cd?cwd=" ++ cwd)
        |> Http.send
            (\res ->
                SetCwd <|
                    Result.mapError httpErrorMessage res
            )


sendMkDir : String -> String -> Cmd Msg
sendMkDir url path =
    Http.getString (url ++ "/mkdir?path=" ++ path)
        |> Http.send
            (Result.mapError httpErrorMessage
                >> Result.map (always ())
                >> MakeDir
            )


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
