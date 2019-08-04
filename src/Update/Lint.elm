module Update.Lint exposing (onLint, sendLintOnTheFly, sendLintProject)

import Helper.Helper exposing (..)
import Http
import Internal.Position
    exposing
        ( Position
        , endPositionDecoder
        , positionDecoder
        , regionDecoder
        )
import Internal.TextBuffer as B
import Json.Decode as Decode exposing (decodeString)
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.Lint exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Regex as Re
import Update.Message exposing (BufferIdentifier, Msg(..))
import Update.Service exposing (post)


applyLintItems : List LintError -> Buffer -> Global -> Global
applyLintItems items buf global =
    let
        normalizeFile file =
            if
                String.isEmpty file
                    || String.endsWith buf.path file
                    || String.endsWith
                        "912ec803b2ce49e4a541068d495ab570.elm"
                        file
            then
                buf.path

            else
                normalizePath global.pathSeperator file

        normalizeRegion ( b, e_ ) =
            ( b
            , -- make inclusive
              if b == e_ then
                e_

              else
                Tuple.mapSecond
                    (\x -> x - 1)
                    e_
            )

        items1 =
            List.map
                (\item ->
                    { item
                        | file = normalizeFile item.file
                        , region = normalizeRegion item.region
                    }
                )
                items
    in
    { global
        | lint =
            { items = items1
            , count = List.length items1
            }
        , locationList =
            lintErrorToLocationList items1
    }


onLint : BufferIdentifier -> Result a (List LintError) -> Buffer -> Global -> Global
onLint ( bufId, version ) resp buf global =
    if
        (bufId == buf.id)
            && (version == buf.history.version)
    then
        case resp of
            Ok items ->
                applyLintItems items buf global

            _ ->
                global

    else
        global


sendLintProject : String -> String -> String -> String -> Int -> B.TextBuffer -> Cmd Msg
sendLintProject url sep bufId path version lines =
    Http.getString (url ++ "/lint?path=" ++ path)
        |> Http.send (parseLintResult sep path lines >> Lint ( bufId, version ))


sendLintOnTheFly : String -> String -> String -> String -> Int -> B.TextBuffer -> Cmd Msg
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
                    Err <| httpErrorMessage err


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
                                (decodeString (elmMakeResultDecoder lines)
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
                                          , details = [ PlainText s ]
                                          }
                                        ]
                                    )

                _ ->
                    Ok []

        Err err ->
            Err (httpErrorMessage err)


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
                    |> Decode.map (PlainText >> List.singleton)
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


elmMakeResultDecoder : B.TextBuffer -> Decode.Decoder (List LintError)
elmMakeResultDecoder lines =
    let
        messageDecoder =
            Decode.list
                (Decode.oneOf
                    [ Decode.map PlainText Decode.string
                    , Decode.map4
                        (\bold color underline s ->
                            RichText
                                { bold = bold
                                , color = color
                                , underline = underline
                                , string = s
                                }
                        )
                        (Decode.field "bold" Decode.bool)
                        (Decode.field "color" Decode.string |> Decode.maybe)
                        (Decode.field "underline" Decode.bool)
                        (Decode.field "string" Decode.string)
                    ]
                )

        compileErrorDecoder path =
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
                (Decode.field "message" messageDecoder)
                (Decode.field "region" regionDecoder)

        errorDecoder =
            let
                firstChar =
                    ( ( 0, 0 ), ( 0, 1 ) )

                formatUnknownImportError title messages error =
                    if title == "UNKNOWN IMPORT" then
                        case messages of
                            _ :: (RichText { string }) :: rest ->
                                { error
                                    | region =
                                        lines
                                            |> findFirstLineStartsWith string
                                            |> Maybe.withDefault firstChar
                                    , details = rest
                                }

                            _ ->
                                error

                    else
                        error
            in
            Decode.map3
                (\path title messages ->
                    { tipe = "error"
                    , tag = Nothing
                    , file = path
                    , overview = title
                    , details = messages
                    , region = firstChar
                    , subRegion = Nothing
                    }
                        |> formatUnknownImportError title messages
                )
                (Decode.field "path" Decode.string)
                (Decode.field "title" Decode.string)
                (Decode.field "message" messageDecoder)
    in
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typ ->
                if typ == "compile-errors" then
                    Decode.field "errors"
                        (Decode.field "path" Decode.string
                            |> Decode.andThen
                                (\path ->
                                    compileErrorDecoder path
                                        |> Decode.list
                                        |> Decode.field "problems"
                                )
                            |> Decode.list
                        )
                        |> Decode.map List.concat

                else if typ == "error" then
                    Decode.map List.singleton errorDecoder

                else
                    Decode.succeed []
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
                , details = [ PlainText details ]
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
            , details = [ PlainText details ]
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


findFirstLineStartsWith : String -> B.TextBuffer -> Maybe ( Position, Position )
findFirstLineStartsWith s lines =
    B.findFirstLine
        (\line i ->
            if String.startsWith s line then
                Just
                    ( ( i, 0 )
                    , ( i, String.length s )
                    )

            else
                Nothing
        )
        lines
