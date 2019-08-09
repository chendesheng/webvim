module Update.Tokenize exposing
    ( onTokenized
    , sendTokenize
    , sendTokenizeLine
    , sendTokenizeTask
    , tokenizeBufferCmd
    , tokenizeLineCmd
    , tokenizeResponseDecoder
    , tokensParser
    , unpackClass
    )

import Array
import Bitwise as Bit
import Debouncers exposing (..)
import Helper.Helper exposing (httpErrorMessage, httpJsonResponse)
import Http
import Internal.Syntax exposing (Token, TokenType(..))
import Internal.TextBuffer as B
import Json.Decode as Decode
import Model exposing (Editor)
import Model.Buffer exposing (..)
import Task exposing (Task)
import Update.Buffer as Buf
import Update.Cursor exposing (pairCursor)
import Update.Message exposing (..)


tokenizeLineCmd : String -> Int -> Buffer -> Cmd Msg
tokenizeLineCmd url begin buf =
    if buf.config.syntax then
        let
            view =
                buf.view
        in
        case B.getLine begin buf.lines of
            Just line ->
                sendTokenizeLine url
                    { bufId = buf.id
                    , path = buf.path
                    , version = buf.history.version
                    , line = begin
                    , lines = line
                    }

            _ ->
                Cmd.none

    else
        Cmd.none


tokenizeBufferCmd : Int -> String -> Buffer -> Cmd Msg
tokenizeBufferCmd begin url buf =
    if buf.config.syntax then
        let
            view =
                buf.view

            scrollTop =
                Buf.finalScrollTop buf

            scrollBottom =
                scrollTop + view.size.height

            end =
                scrollBottom + view.size.height
        in
        if begin < scrollBottom then
            let
                lines =
                    buf.lines
                        |> B.sliceLines begin end
                        |> B.toString
            in
            if String.isEmpty lines then
                Cmd.none

            else
                sendTokenize
                    url
                    { bufId = buf.id
                    , path = buf.path
                    , version = buf.history.version
                    , line = begin
                    , lines = lines
                    }

        else
            Cmd.none

    else
        Cmd.none


sendTokenizeTask :
    String
    -> TokenizeRequest
    -> Task String TokenizeResponse
sendTokenizeTask url { bufId, version, path, line, lines } =
    Http.task
        { method = "Post"
        , body = Http.stringBody "text/plain" lines
        , headers = []
        , timeout = Nothing
        , url =
            url
                ++ "/tokenize?path="
                ++ path
                ++ "&line="
                ++ String.fromInt line
        , resolver =
            Http.stringResolver <|
                httpJsonResponse (tokenizeResponseDecoder line)
                    >> Result.map Tuple.second
                    >> Result.mapError httpErrorMessage
        }


sendTokenize : String -> TokenizeRequest -> Cmd Msg
sendTokenize url ({ bufId, version, path, line, lines } as req) =
    Http.post
        { url =
            url
                ++ "/tokenize?path="
                ++ path
                ++ "&line="
                ++ String.fromInt line
        , body = Http.stringBody "text/plain" lines
        , expect =
            Http.expectJson
                (Result.mapError httpErrorMessage
                    >> Tokenized ( bufId, version )
                )
                (tokenizeResponseDecoder line)
        }



--Task.attempt (Tokenized ( bufId, version )) (sendTokenizeTask url req)


sendTokenizeLine : String -> TokenizeRequest -> Cmd Msg
sendTokenizeLine url req =
    Cmd.map
        (\msg ->
            case msg of
                Tokenized bufId res ->
                    res
                        |> Result.map
                            (\r ->
                                case r of
                                    TokenizeSuccess n syntax ->
                                        case Array.get 0 syntax of
                                            Just tokens ->
                                                TokenizeLineSuccess n tokens

                                            _ ->
                                                r

                                    _ ->
                                        r
                            )
                        |> Tokenized bufId

                _ ->
                    msg
        )
        (sendTokenize url req)


tokenizeResponseDecoder : Int -> Decode.Decoder TokenizeResponse
tokenizeResponseDecoder n =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\tipe ->
                case tipe of
                    "success" ->
                        Decode.map2
                            (\path list ->
                                list
                                    |> Array.fromList
                                    |> TokenizeSuccess n
                            )
                            (Decode.field "path" Decode.string)
                            (Decode.field "payload" (Decode.list tokensParser))

                    "error" ->
                        Decode.field "payload" Decode.string
                            |> Decode.map TokenizeError

                    _ ->
                        Decode.succeed TokenizeCacheMiss
            )


tokensParser : Decode.Decoder (List Token)
tokensParser =
    Decode.list Decode.int
        |> Decode.map
            (\tokens ->
                let
                    ( indexes, classes ) =
                        tokens
                            |> List.map2 Tuple.pair
                                (List.range 0 <|
                                    List.length tokens
                                )
                            |> List.partition (Tuple.first >> (\n -> modBy 2 n == 0))
                            |> (\item ->
                                    let
                                        ( a, b ) =
                                            item
                                    in
                                    ( List.map Tuple.second a
                                    , List.map Tuple.second b
                                    )
                               )

                    indexes2 =
                        indexes
                            |> List.tail
                            |> Maybe.withDefault []
                in
                List.map3
                    unpackClass
                    indexes
                    indexes2
                    classes
            )


unpackClass : Int -> Int -> Int -> Token
unpackClass startIndex endIndex n =
    let
        languageid_mask =
            255

        token_type_mask =
            1792

        font_style_mask =
            14336

        foreground_mask =
            8372224

        background_mask =
            4286578688

        languageid_offset =
            0

        token_type_offset =
            8

        font_style_offset =
            11

        foreground_offset =
            14

        background_offset =
            23

        foreground =
            n
                |> Bit.and foreground_mask
                |> Bit.shiftRightZfBy foreground_offset

        italic =
            1

        bold =
            2

        underline =
            4

        fontStyle =
            n
                |> Bit.and font_style_mask
                |> Bit.shiftRightZfBy font_style_offset
                |> (\style ->
                        if Bit.and style italic > 0 then
                            " mtki"

                        else
                            ""
                                ++ (if Bit.and style bold > 0 then
                                        " mtkb"

                                    else
                                        ""
                                            ++ (if Bit.and style underline > 0 then
                                                    " mtku"

                                                else
                                                    ""
                                               )
                                   )
                   )

        tokenType =
            n
                |> Bit.and token_type_mask
                |> Bit.shiftRightZfBy token_type_offset
    in
    { length = endIndex - startIndex
    , classname = "mtk" ++ String.fromInt foreground ++ fontStyle
    , tipe =
        case tokenType of
            1 ->
                TokenComment

            2 ->
                TokenString

            4 ->
                TokenRegex

            _ ->
                TokenOther
    }


onTokenized : Editor -> Result error TokenizeResponse -> ( Editor, Cmd Msg )
onTokenized ({ buf, global } as ed) resp =
    case resp of
        Ok payload ->
            case payload of
                TokenizeSuccess n syntax ->
                    ( let
                        syntax1 =
                            buf.syntax
                                |> Array.slice 0 n
                                |> (\s -> Array.append s syntax)
                      in
                      { ed
                        | buf =
                            { buf
                                | syntax = syntax1
                                , syntaxDirtyFrom = Array.length syntax1
                                , view =
                                    pairCursor
                                        buf.mode
                                        buf.lines
                                        buf.syntax
                                        buf.view
                            }
                      }
                    , Cmd.none
                    )

                TokenizeLineSuccess begin tokens ->
                    let
                        ( debouncers, cmd ) =
                            debounceTokenize
                                Debouncing
                                ed.global.debouncers
                                100
                    in
                    ( { ed
                        | buf =
                            { buf
                                | syntax =
                                    Array.set begin tokens buf.syntax
                                , syntaxDirtyFrom = begin + 1
                            }
                        , global = { global | debouncers = debouncers }
                      }
                    , cmd
                    )

                TokenizeCacheMiss ->
                    tokenizeBuffer
                        { ed
                            | buf =
                                { buf
                                    | syntax = Array.empty
                                    , syntaxDirtyFrom = 0
                                }
                        }

                TokenizeError _ ->
                    let
                        config =
                            buf.config
                    in
                    ( { ed
                        | buf =
                            { buf
                                | syntax = Array.empty
                                , syntaxDirtyFrom = 0
                                , config = { config | syntax = False }
                            }
                      }
                    , Cmd.none
                    )

        Err _ ->
            ( ed, Cmd.none )


tokenizeBuffer : Editor -> ( Editor, Cmd Msg )
tokenizeBuffer ed =
    ( ed, tokenizeBufferCmd ed.buf.syntaxDirtyFrom ed.global.service ed.buf )
