module Service exposing (..)

import Http
import Message
    exposing
        ( Msg(..)
        , BufferInfo
        , LintError
        , elmMakeResultDecoder
        , TokenizeResponse(..)
        , File
        , TokenizeRequest
        )
import Json.Decode as Decode exposing (decodeString)
import Elm.Array as Array
import Bitwise as Bit
import Syntax exposing (Token)
import List
import Parser as P exposing ((|.), (|=), Parser)
import Char


sendEditBuffer : String -> BufferInfo -> Cmd Msg
sendEditBuffer url info =
    url
        ++ "/edit?path="
        ++ info.path
        |> Http.getString
        |> Http.send
            (Result.map
                (\s ->
                    { info | content = Just s }
                )
                >> Read
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


sendSaveBuffer : String -> String -> String -> Cmd Msg
sendSaveBuffer url path buf =
    let
        body =
            Http.stringBody "text/plain" buf
    in
        (post
            (url ++ "/write?path=" ++ path)
            body
        )
            |> Http.send Write


syntaxErrorParser : Parser LintError
syntaxErrorParser =
    P.succeed
        (\tag file overview y x details ->
            let
                y1 =
                    y - 1

                x1 =
                    x - (toString y1 ++ "| " |> String.length)
            in
                { tipe = "error"
                , tag = tag
                , file = file
                , overview = overview
                , details = details
                , region = ( ( y1, x1 ), ( y1, x1 ) )
                , subRegion = Nothing
                }
        )
        |. P.ignore P.oneOrMore (\c -> c == '-' || c == ' ')
        |= ((P.succeed identity
                |. P.keep P.oneOrMore Char.isUpper
                |. P.keep P.oneOrMore ((==) ' ')
                |. P.keep P.oneOrMore Char.isUpper
            )
                |> P.source
           )
        |. P.ignore P.oneOrMore (\c -> c == '-' || c == ' ')
        |= P.keep P.oneOrMore ((/=) '\n')
        |= (P.keep P.zeroOrMore (Char.isDigit >> not)
                |> P.source
                |> P.map String.trim
           )
        |= (P.keep P.oneOrMore Char.isDigit
                |> P.map (String.toInt >> Result.withDefault 0)
           )
        |. P.ignoreUntil "\n"
        |= (P.keep P.zeroOrMore ((/=) '^') |> P.source |> P.map String.length)
        |. P.ignoreUntil "\n"
        |= (P.keep P.zeroOrMore (always True)
                |> P.source
                |> P.map String.trim
           )


parseLintResponse : Result a String -> Result String (List LintError)
parseLintResponse resp =
    case Result.mapError toString resp of
        Ok s ->
            if String.startsWith "[" s then
                decodeString elmMakeResultDecoder s
            else
                case P.run syntaxErrorParser s of
                    Ok item ->
                        Ok [ item ]

                    Err _ ->
                        Ok
                            [ { tipe = "error"
                              , tag = ""
                              , file = ""
                              , overview = ""
                              , details = s
                              , region = ( ( 0, 0 ), ( 0, 0 ) )
                              , subRegion = Nothing
                              }
                            ]

        Err err ->
            Err err


sendLintProject : String -> String -> Cmd Msg
sendLintProject url path =
    Http.getString (url ++ "/lint?path=" ++ path)
        |> Http.send (parseLintResponse >> Lint)


sendLintOnTheFly : String -> String -> String -> Cmd Msg
sendLintOnTheFly url path buf =
    let
        body =
            Http.stringBody "text/plain" buf
    in
        post (url ++ "/lint?path=" ++ path) body
            |> Http.send (parseLintResponse >> LintOnTheFly)


unpackClass : Int -> String
unpackClass n =
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
                |> Bit.shiftRightBy foreground_offset

        fontStyle =
            n
                |> Bit.and font_style_mask
                |> Bit.shiftRightBy font_style_offset
                |> (\style ->
                        case style of
                            1 ->
                                " mtki"

                            2 ->
                                " mtkb"

                            _ ->
                                ""
                   )
    in
        "mtk" ++ (toString foreground) ++ fontStyle


tokensParser : Decode.Decoder (List Token)
tokensParser =
    Decode.list Decode.int
        |> Decode.map
            (\tokens ->
                let
                    ( indexes, classes ) =
                        tokens
                            |> List.map2 (,)
                                (List.range 0 <|
                                    List.length tokens
                                )
                            |> List.partition (Tuple.first >> \n -> n % 2 == 0)
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
                    (List.map3
                        (\startIndex endIndex class ->
                            { length = endIndex - startIndex
                            , classname = unpackClass class
                            }
                        )
                        indexes
                        indexes2
                        classes
                    )
            )


tokenizeResponseDecoder : Int -> Decode.Decoder TokenizeResponse
tokenizeResponseDecoder n =
    (Decode.field "type" Decode.string)
        |> Decode.andThen
            (\tipe ->
                case tipe of
                    "success" ->
                        Decode.map3
                            (\path version list ->
                                list
                                    |> Array.fromList
                                    |> TokenizeSuccess path version n
                            )
                            (Decode.field "path" Decode.string)
                            (Decode.field "version" Decode.int)
                            (Decode.field "payload" (Decode.list tokensParser))

                    _ ->
                        Decode.succeed TokenizeCacheMiss
            )


sendTokenize : String -> TokenizeRequest -> Cmd Msg
sendTokenize url { path, version, line, lines } =
    let
        body =
            Http.stringBody "text/plain" lines
    in
        if path == "" then
            Cmd.none
        else
            tokenizeResponseDecoder line
                |> Http.post
                    (url
                        ++ "/tokenize?path="
                        ++ path
                        ++ "&line="
                        ++ (toString line)
                        ++ "&version="
                        ++ (toString version)
                    )
                    body
                |> Http.send Tokenized


sendTokenizeLine : String -> TokenizeRequest -> Cmd Msg
sendTokenizeLine url req =
    Cmd.map
        (\msg ->
            case msg of
                Tokenized res ->
                    res
                        |> Result.map
                            (\r ->
                                case r of
                                    TokenizeSuccess path version i syntax ->
                                        case Array.get 0 syntax of
                                            Just tokens ->
                                                LineTokenizeSuccess
                                                    path
                                                    version
                                                    i
                                                    tokens

                                            _ ->
                                                r

                                    _ ->
                                        r
                            )
                        |> Tokenized

                _ ->
                    msg
        )
        (sendTokenize url req)


parseFileList : Result a String -> Result String (List File)
parseFileList resp =
    case resp of
        Ok s ->
            s
                |> String.split "\n"
                |> List.map String.trim
                |> Ok

        Err err ->
            Err <| toString err


sendListFiles : String -> Cmd Msg
sendListFiles url =
    Http.getString (url ++ "/ls")
        |> Http.send (parseFileList >> ListFiles)
