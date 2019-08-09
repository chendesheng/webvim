module Update.Service exposing
    ( lineDiffDecoder
    , sendCd
    , sendListAllFiles
    , sendListDirs
    , sendListFiles
    , sendMkDir
    , sendReadBuffer
    , sendSearch
    , sendWriteBuffer
    )

import Array as Array
import Dict exposing (Dict)
import Fs exposing (FileSystem)
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


sendReadBuffer : String -> FileSystem -> Int -> Win.Path -> Buffer -> Cmd Msg
sendReadBuffer url fs viewHeight framePath buf =
    Fs.read fs buf.path
        |> Task.andThen
            (\result ->
                case result of
                    Just ( lastModified, body ) ->
                        let
                            s =
                                if String.endsWith lineBreak body then
                                    body

                                else
                                    body ++ lineBreak

                            lines =
                                B.fromStringExpandTabs buf.config.tabSize 0 s

                            tokenizeLines =
                                Tuple.first buf.view.cursor + viewHeight * 2

                            response =
                                { syntaxEnabled = buf.config.syntax
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

                                            TokenizeError "noextension" ->
                                                { response | syntaxEnabled = False }

                                            _ ->
                                                response
                                    )
                                |> Task.onError (\_ -> Task.succeed response)

                        else
                            Task.succeed response

                    _ ->
                        Task.succeed
                            { syntaxEnabled = buf.config.syntax
                            , lines = B.empty
                            , syntax = Array.empty
                            , lastModified = ""
                            }
            )
        |> Task.attempt
            (\result ->
                case result of
                    Ok { syntax, syntaxEnabled, lines, lastModified } ->
                        let
                            config =
                                buf.config

                            history =
                                if buf.history.lastModified == lastModified then
                                    buf.history

                                else
                                    { emptyBufferHistory | lastModified = lastModified }
                        in
                        ( framePath
                        , { buf
                            | lines = lines
                            , config = { config | syntax = syntaxEnabled }
                            , syntax = syntax
                            , syntaxDirtyFrom = Array.length syntax
                            , history = history
                          }
                        )
                            |> Ok
                            |> Read

                    Err err ->
                        Read (Err err)
            )


sendWriteBuffer : FileSystem -> String -> Buffer -> Cmd Msg
sendWriteBuffer fs path buf =
    Fs.write fs path (B.toString buf.lines)
        |> Cmd.map
            (Result.andThen
                (\( lastModified, content ) ->
                    case Decode.decodeString lineDiffDecoder content of
                        Ok patches ->
                            Ok ( lastModified, patches )

                        Err _ ->
                            Err "decode write response error"
                )
                >> Write
            )


sendListAllFiles : FileSystem -> Cmd Msg
sendListAllFiles fs =
    Fs.listAllFiles fs |> Cmd.map ListAllFiles


sendListFiles : FileSystem -> String -> Cmd Msg
sendListFiles fs cwd =
    Fs.listFiles fs cwd
        |> Cmd.map ListFiles


sendListDirs : FileSystem -> String -> Cmd Msg
sendListDirs fs cwd =
    Fs.listDirs fs cwd
        |> Cmd.map ListDirs


sendSearch : FileSystem -> String -> Cmd Msg
sendSearch fs s =
    Fs.search fs s
        |> Cmd.map SearchResult


sendCd : FileSystem -> String -> Cmd Msg
sendCd fs cwd =
    Fs.cd fs cwd
        |> Cmd.map SetCwd


sendMkDir : FileSystem -> String -> Cmd Msg
sendMkDir fs path =
    Fs.mkDir fs path
        |> Cmd.map MakeDir
