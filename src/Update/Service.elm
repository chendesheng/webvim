module Update.Service exposing
    ( bootDecoder
    , cannotFindModuleError
    , ctagsParser
    , elmMakeResultDecoder
    , errorMessage
    , eslintResultDecoder
    , getBodyAndHeaders
    , lineDiffDecoder
    , parseElmMakeResponse
    , parseFileList
    , parseLintResult
    , pickClosest
    , post
    , postRespHeaders
    , sendBoot
    , sendCd
    , sendLintOnTheFly
    , sendLintProject
    , sendListAllFiles
    , sendListDirectories
    , sendListFiles
    , sendMkDir
    , sendReadBuffer
    , sendReadClipboard
    , sendReadTags
    , sendSearch
    , sendTokenize
    , sendTokenizeLine
    , sendTokenizeTask
    , sendWriteBuffer
    , sendWriteClipboard
    , syntaxErrorParser
    , tokenizeResponseDecoder
    , tokensParser
    , unpackClass
    )

import Array as Array
import Bitwise as Bit
import Char
import Dict exposing (Dict)
import Helper.Helper
    exposing
        ( chompUntilAfter
        , isSpace
        , joinPath
        , keepOneOrMore
        , keepZeroOrMore
        , normalizePath
        , notSpace
        , oneOrMore
        , regex
        )
import Http
import Internal.Jumps exposing (Location)
import Internal.Position
    exposing
        ( endPositionDecoder
        , positionDecoder
        , regionDecoder
        )
import Internal.Syntax exposing (Token, TokenType(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Json.Decode as Decode exposing (decodeString)
import List
import Model
    exposing
        ( Buffer
        , Flags
        , Key
        , LintError
        , RichText(..)
        , TextWithStyle
        , emptyBufferHistory
        )
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
import Vim.AST exposing (AST)


eslintResultDecoder : Decode.Decoder (List LintError)
eslintResultDecoder =
    let
        regionDecoder =
            positionDecoder
                |> Decode.andThen
                    (\start ->
                        Decode.map
                            (Tuple.pair start)
                            endPositionDecoder
                            |> Decode.maybe
                            |> Decode.map (Maybe.withDefault ( start, start ))
                    )

        messageDecoder filePath =
            Decode.map7 LintError
                (Decode.field "severity" Decode.int
                    |> Decode.map
                        (\severity ->
                            if severity == 2 then
                                "error"

                            else
                                "warning"
                        )
                )
                (Decode.succeed Nothing)
                (Decode.succeed filePath)
                (Decode.field "ruleId"
                    (Decode.oneOf
                        [ Decode.null ""
                        , Decode.string
                            |> Decode.map (\s -> "(" ++ s ++ ")")
                        ]
                    )
                )
                (Decode.field "message" Decode.string
                    |> Decode.map PlainText
                )
                regionDecoder
                (Decode.succeed Nothing)
                |> Decode.list
    in
    Decode.field "filePath" Decode.string
        |> Decode.andThen
            (\filePath ->
                Decode.field "messages" (messageDecoder filePath)
            )
        |> Decode.list
        |> Decode.map List.concat


elmMakeResultDecoder : Decode.Decoder (List LintError)
elmMakeResultDecoder =
    let
        messageDecoder =
            Decode.list
                (Decode.oneOf
                    [ Decode.string
                        |> Decode.map
                            (\s ->
                                { bold = False
                                , color = Nothing
                                , underline = False
                                , string = s
                                }
                            )
                    , Decode.map4 TextWithStyle
                        (Decode.field "bold" Decode.bool)
                        (Decode.field "color" Decode.string |> Decode.maybe)
                        (Decode.field "underline" Decode.bool)
                        (Decode.field "string" Decode.string)
                    ]
                )

        errorDecoder path =
            Decode.map3
                (\title details region ->
                    { tipe = "error"
                    , tag = Nothing
                    , file = path
                    , overview = title
                    , details = details
                    , region = region
                    , subRegion = Nothing
                    }
                )
                (Decode.field "title" Decode.string)
                (Decode.field "message" messageDecoder |> Decode.map RichText)
                (Decode.field "region" regionDecoder)
    in
    Decode.field "errors"
        (Decode.field "path" Decode.string
            |> Decode.andThen
                (\path ->
                    errorDecoder path
                        |> Decode.list
                        |> Decode.field "problems"
                )
            |> Decode.list
        )
        |> Decode.map List.concat


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


sendReadBuffer : String -> Int -> Bool -> Buffer -> Cmd Msg
sendReadBuffer url viewHeight setActive buf =
    url
        ++ "/read?path="
        ++ buf.path
        |> getBodyAndHeaders
        |> Http.toTask
        |> Task.andThen
            (\( headers, s ) ->
                let
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
                        Read (Ok ( setActive, b ))

                    Err ((Http.BadStatus resp) as err) ->
                        case resp.status.code of
                            -- 404 is ok here, the buffer is newly created
                            404 ->
                                Read (Ok ( setActive, buf ))

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
                        Write <| Err <| errorMessage s
            )


cannotFindModuleError : String -> B.TextBuffer -> Parser LintError
cannotFindModuleError path lines =
    let
        findRegion name lines_ =
            lines_
                |> B.findFirstLine
                    (\line i ->
                        line
                            |> P.run
                                (P.succeed identity
                                    |. P.symbol "import"
                                    |. chompUntilAfter name
                                    |. P.chompIf isSpace
                                    |. P.chompWhile isSpace
                                )
                            |> Result.toMaybe
                            |> Maybe.map (always i)
                    )
                |> Maybe.map
                    (\y ->
                        ( ( y, 0 )
                        , ( y, "import " ++ name |> String.length )
                        )
                    )
                |> Maybe.withDefault ( ( 0, 0 ), ( 0, 0 ) )
    in
    (P.succeed
        identity
        |. P.symbol "I cannot find module '"
        |= keepOneOrMore (\c -> c /= '\'')
        |. P.chompWhile (always True)
        |. P.end
    )
        |> P.mapChompedString
            (\name details ->
                { tipe = "error"
                , tag = Nothing
                , file = path
                , overview = ""
                , details = PlainText details
                , region = findRegion name lines
                , subRegion = Nothing
                }
            )


syntaxErrorParser : Parser LintError
syntaxErrorParser =
    P.succeed
        (\tag file overview y x details ->
            let
                y1 =
                    y - 1

                x1 =
                    x - (String.fromInt y1 ++ "| " |> String.length)
            in
            { tipe = "error"
            , tag = Just tag
            , file = file
            , overview = overview
            , details = PlainText details
            , region = ( ( y1, x1 ), ( y1, x1 + 1 ) )
            , subRegion = Nothing
            }
        )
        |. oneOrMore (\c -> c == '-' || c == ' ')
        |= (oneOrMore Char.isUpper
                |. oneOrMore ((==) ' ')
                |. oneOrMore Char.isUpper
                |> P.getChompedString
           )
        |. oneOrMore (\c -> c == '-' || c == ' ')
        |= keepOneOrMore ((/=) '\n')
        |= (keepZeroOrMore (not << Char.isDigit)
                |> P.map String.trim
           )
        |= (keepOneOrMore Char.isDigit
                |> P.map (String.toInt >> Maybe.withDefault 0)
           )
        |. chompUntilAfter "\n"
        |= (keepZeroOrMore ((/=) '^') |> P.map String.length)
        |. chompUntilAfter "\n"
        |= (keepZeroOrMore (always True)
                |> P.map String.trim
           )


parseElmMakeResponse :
    String
    -> String
    -> B.TextBuffer
    -> Result Http.Error String
    -> Result String (List LintError)
parseElmMakeResponse sep path lines resp =
    case resp of
        Ok ss ->
            case Re.splitAtMost 1 (regex "\n") ss of
                [ dir, s ] ->
                    if String.trim s == "Successfully generated /dev/null" then
                        Ok []

                    else if String.startsWith "{" s then
                        s
                            |> String.lines
                            |> List.map
                                (decodeString elmMakeResultDecoder
                                    >> Result.map
                                        (List.map
                                            (\item ->
                                                { item
                                                    | file =
                                                        joinPath sep
                                                            dir
                                                            item.file
                                                }
                                            )
                                        )
                                    >> Result.withDefault []
                                )
                            |> List.concat
                            |> List.sortBy
                                (\{ file, tipe, region } ->
                                    ( file, tipe, region )
                                )
                            |> Ok

                    else
                        case
                            P.run
                                (P.oneOf
                                    [ syntaxErrorParser
                                    , cannotFindModuleError path lines
                                    ]
                                )
                                s
                        of
                            Ok item ->
                                Ok [ { item | file = joinPath sep dir item.file } ]

                            Err _ ->
                                Ok
                                    (if
                                        s
                                            |> String.trim
                                            |> String.isEmpty
                                     then
                                        []

                                     else
                                        [ { file = path
                                          , tag = Nothing
                                          , tipe = "error"
                                          , region = ( ( 0, 0 ), ( 0, 0 ) )
                                          , subRegion = Nothing
                                          , overview = ""
                                          , details = PlainText s
                                          }
                                        ]
                                    )

                _ ->
                    Ok []

        Err err ->
            Err (errorMessage err)


parseLintResult :
    String
    -> String
    -> B.TextBuffer
    -> Result Http.Error String
    -> Result String (List LintError)
parseLintResult sep path lines =
    if String.endsWith ".elm" path then
        parseElmMakeResponse sep path lines

    else
        \result ->
            case result of
                Ok s ->
                    decodeString eslintResultDecoder s
                        |> Result.mapError Decode.errorToString

                Err err ->
                    Err <| errorMessage err


sendLintProject : String -> String -> Int -> String -> Int -> B.TextBuffer -> Cmd Msg
sendLintProject url sep bufId path version lines =
    Http.getString (url ++ "/lint?path=" ++ path)
        |> Http.send (parseLintResult sep path lines >> Lint ( bufId, version ))


sendLintOnTheFly : String -> String -> Int -> String -> Int -> B.TextBuffer -> Cmd Msg
sendLintOnTheFly url sep bufId path version lines =
    let
        body =
            Http.stringBody "text/plain" <| B.toString lines
    in
    post (url ++ "/lint?path=" ++ path) body
        |> Http.send
            (parseLintResult sep path lines
                >> Lint ( bufId, version )
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


sendTokenizeTask :
    String
    -> TokenizeRequest
    -> Task String TokenizeResponse
sendTokenizeTask url { path, line, lines } =
    tokenizeResponseDecoder line
        |> Http.post
            (url
                ++ "/tokenize?path="
                ++ path
                ++ "&line="
                ++ String.fromInt line
            )
            (Http.stringBody "text/plain" lines)
        |> Http.toTask
        |> Task.mapError errorMessage


sendTokenize : String -> TokenizeRequest -> Cmd Msg
sendTokenize url ({ bufId, version } as req) =
    Task.attempt (Tokenized ( bufId, version )) (sendTokenizeTask url req)


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


parseFileList : String -> Result Http.Error String -> Result String (List String)
parseFileList sep resp =
    case resp of
        Ok s ->
            s
                |> String.split "\n"
                |> List.map (normalizePath sep)
                |> Ok

        Err err ->
            Err <| errorMessage err


sendListAllFiles : String -> String -> String -> Cmd Msg
sendListAllFiles url sep cwd =
    Http.getString (url ++ "/ls?cwd=" ++ cwd)
        |> Http.send (parseFileList sep >> ListAllFiles)


sendListFiles : String -> String -> String -> Cmd Msg
sendListFiles url sep cwd =
    let
        trimSep s =
            if String.endsWith sep s || String.toLower s == ".ds_store" then
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
                    Result.mapError errorMessage result
            )


ctagsParser : String -> Parser (List Location)
ctagsParser sep =
    P.loop []
        (\locations ->
            P.oneOf
                [ P.succeed
                    (\path x y ->
                        P.Loop ({ cursor = ( y - 1, x ), path = normalizePath sep path } :: locations)
                    )
                    |. oneOrMore notSpace
                    |. oneOrMore isSpace
                    |= keepOneOrMore notSpace
                    |= (P.succeed String.length
                            |. chompUntilAfter "/^"
                            |= P.oneOf
                                [ keepOneOrMore isSpace
                                , P.succeed ""
                                ]
                       )
                    |. chompUntilAfter "line:"
                    |= P.int
                    |. chompUntilAfter "\n"
                    |. P.chompWhile isSpace
                , P.succeed (P.Done locations)
                    |. P.end
                ]
        )


pickClosest : String -> Int -> List Location -> Maybe Location
pickClosest path index locs =
    locs
        |> List.sortBy .path
        |> Array.fromList
        |> Array.get index


sendReadTags : String -> String -> String -> String -> Int -> String -> Cmd Msg
sendReadTags url sep cwd path index name =
    Http.getString (url ++ "/readtags?name=" ++ name ++ "&cwd=" ++ cwd)
        |> Http.send
            (\result ->
                result
                    |> Result.mapError errorMessage
                    |> Result.andThen
                        (\s ->
                            P.run (ctagsParser sep) s
                                --|> Debug.log "ctags parse"
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


sendSearch : String -> String -> String -> Cmd Msg
sendSearch url cwd s =
    Http.getString (url ++ "/search?s=" ++ s ++ "&cwd=" ++ cwd)
        |> Http.send
            (\res ->
                SearchResult <|
                    Result.mapError errorMessage res
            )


sendCd : String -> String -> Cmd Msg
sendCd url cwd =
    Http.getString (url ++ "/cd?cwd=" ++ cwd)
        |> Http.send
            (\res ->
                SetCwd <|
                    Result.mapError errorMessage res
            )


sendMkDir : String -> String -> Cmd Msg
sendMkDir url path =
    Http.getString (url ++ "/mkdir?path=" ++ path)
        |> Http.send
            (Result.mapError errorMessage
                >> Result.map (always ())
                >> MakeDir
            )


bootDecoder : Flags -> Decode.Decoder Flags
bootDecoder flags =
    Decode.map2
        (\homedir pathSeperator ->
            { flags
                | cwd =
                    if String.isEmpty flags.cwd then
                        homedir

                    else
                        flags.cwd
                , pathSeperator = pathSeperator
                , homedir = homedir
            }
        )
        (Decode.field "homedir" Decode.string)
        (Decode.field "pathSeperator" Decode.string)


sendBoot : Flags -> Cmd Msg
sendBoot ({ service } as flags) =
    Http.get (service ++ "/boot") (bootDecoder flags)
        |> Http.send
            (\result ->
                Boot <|
                    Result.mapError errorMessage result
            )


errorMessage : Http.Error -> String
errorMessage err =
    case err of
        Http.BadUrl s ->
            "BadUrl: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus { status } ->
            "NetworkError: " ++ String.fromInt status.code

        Http.BadPayload s _ ->
            "BadPayload: " ++ s
