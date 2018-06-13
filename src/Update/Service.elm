module Update.Service exposing (..)

import Http
import Update.Message
    exposing
        ( Msg(..)
        , BufferInfo
        , LintError
        , elmMakeResultDecoder
        , TokenizeResponse(..)
        , File
        , TokenizeRequest
        , Key
        )
import Json.Decode as Decode exposing (decodeString)
import Elm.Array as Array
import Bitwise as Bit
import Internal.Syntax exposing (Token, TokenType(..))
import List
import Parser as P exposing ((|.), (|=), Parser)
import Char
import Vim.AST exposing (AST)
import Internal.Jumps exposing (Location)
import Helper.Helper exposing (levenshtein, isSpace, notSpace)
import Internal.TextBuffer as B
import Task exposing (Task)
import Regex as Re


sendReadBuffer : String -> Int -> Int -> BufferInfo -> Cmd Msg
sendReadBuffer url tokenizeLines tabSize info =
    url
        ++ "/read?path="
        ++ info.path
        |> Http.getString
        |> Http.toTask
        |> Task.andThen
            (\s ->
                let
                    lines =
                        B.fromStringExpandTabs tabSize 0 s
                in
                    sendTokenizeTask url
                        { path = info.path
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
                                    TokenizeSuccess _ _ _ syntax ->
                                        ( lines, syntax )

                                    _ ->
                                        ( lines, Array.empty )
                            )
                        |> Task.onError
                            (\_ ->
                                Task.succeed ( lines, Array.empty )
                            )
            )
        |> Task.attempt
            (Result.map
                (\res ->
                    { info | content = Just res }
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


sendWriteBuffer : String -> Int -> String -> String -> Cmd Msg
sendWriteBuffer url tokenizeLines path buf =
    let
        body =
            Http.stringBody "text/plain" buf
    in
        (post
            (url ++ "/write?path=" ++ path)
            body
        )
            |> Http.toTask
            |> Task.andThen
                (\s ->
                    if s == "" then
                        Task.succeed ( B.empty, Array.empty )
                    else
                        let
                            lines =
                                B.fromString s
                        in
                            sendTokenizeTask url
                                { path = path
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
                                            TokenizeSuccess s _ _ syntax ->
                                                ( lines, syntax )

                                            _ ->
                                                ( lines, Array.empty )
                                    )
                                |> Task.onError
                                    (\_ ->
                                        Task.succeed ( lines, Array.empty )
                                    )
                )
            |> Task.attempt Write


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
        Ok ss ->
            case Re.split (Re.AtMost 1) (Re.regex "\n") ss of
                [ dir, s ] ->
                    if String.trim s == "Successfully generated /dev/null" then
                        Ok []
                    else if String.startsWith "[" s then
                        s
                            |> decodeString elmMakeResultDecoder
                            |> Result.map
                                (List.map
                                    (\item ->
                                        { item | file = dir ++ item.file }
                                    )
                                )
                    else
                        case
                            P.run syntaxErrorParser s
                        of
                            Ok item ->
                                Ok [ { item | file = dir ++ item.file } ]

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

                _ ->
                    Ok []

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
                                ++ if Bit.and style bold > 0 then
                                    " mtkb"
                                   else
                                    ""
                                        ++ if Bit.and style underline > 0 then
                                            " mtku"
                                           else
                                            ""
                   )

        tokenType =
            n
                |> Bit.and token_type_mask
                |> Bit.shiftRightZfBy token_type_offset
    in
        { length = endIndex - startIndex
        , classname = "mtk" ++ (toString foreground) ++ fontStyle
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
                        unpackClass
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


sendTokenizeTask : String -> TokenizeRequest -> Task Http.Error TokenizeResponse
sendTokenizeTask url { path, version, line, lines } =
    let
        body =
            Http.stringBody "text/plain" lines
    in
        if path == "" then
            Task.succeed (TokenizeSuccess "" 0 0 Array.empty)
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
                |> Http.toTask


sendTokenize : String -> TokenizeRequest -> Cmd Msg
sendTokenize url req =
    Task.attempt Tokenized (sendTokenizeTask url req)


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


sendReadClipboard : Bool -> Key -> String -> AST -> Cmd Msg
sendReadClipboard replaying key url op =
    Http.getString (url ++ "/clipboard")
        |> Http.send
            (Result.map (\s -> ( replaying, key, op, s ))
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
        |> Http.send WriteClipboard


ctagsParser : Parser (List Location)
ctagsParser =
    P.succeed
        (\path x y ->
            { cursor = ( y - 1, x ), path = path }
        )
        |. P.ignore P.zeroOrMore isSpace
        |. P.ignore P.oneOrMore notSpace
        |. P.ignore P.oneOrMore isSpace
        |= P.keep P.oneOrMore notSpace
        |= (P.succeed String.length
                |. P.ignoreUntil "/^"
                |= P.oneOf
                    [ P.keep P.oneOrMore isSpace
                    , P.succeed ""
                    ]
           )
        |. P.ignoreUntil "line:"
        |= P.int
        |. P.ignoreUntil "\n"
        |> P.repeat P.oneOrMore


pickClosest : String -> Int -> List Location -> Maybe Location
pickClosest path index locs =
    locs
        |> List.sortBy
            (\loc ->
                levenshtein path loc.path
            )
        |> Array.fromList
        |> Array.get index


sendReadTags : String -> String -> Int -> String -> Cmd Msg
sendReadTags url path index name =
    Http.getString (url ++ "/readtags?name=" ++ name)
        |> Http.send
            (\result ->
                result
                    |> Result.mapError toString
                    |> Result.andThen
                        (\s ->
                            P.run ctagsParser s
                                |> Result.mapError
                                    (always "parse result error")
                                |> Result.andThen
                                    (\locs ->
                                        locs
                                            |> pickClosest path index
                                            |> Result.fromMaybe
                                                "parse result error"
                                    )
                        )
                    |> ReadTags
            )


sendSearch : String -> String -> Cmd Msg
sendSearch url s =
    Http.getString (url ++ "/search?s=" ++ s)
        |> Http.send SearchResult
