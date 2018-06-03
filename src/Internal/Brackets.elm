module Internal.Brackets exposing (..)

import Regex as Re exposing (Regex, regex)
import Internal.Syntax
    exposing
        ( Syntax
        , Token
        , splitTokens
        , getToken
        , TokenType(..)
        , iterateTokens
        )
import Internal.TextBuffer as B exposing (TextBuffer)
import Internal.Position exposing (Position)
import Parser as P exposing ((|.), (|=), Parser)


isBracket : Char -> Bool
isBracket c =
    List.any ((==) c)
        [ '(', ')', '[', ']', '{', '}', '<', '>' ]


bracketsParser : Parser Int
bracketsParser =
    P.succeed String.length
        |= P.keep P.zeroOrMore (isBracket >> not)
        |. P.keep (P.Exactly 1) isBracket


bracket : String -> Maybe ( String, Regex, Bool )
bracket s =
    case s of
        "(" ->
            Just ( ")", regex "[\\(\\)]", True )

        ")" ->
            Just ( "(", regex "[\\(\\)]", False )

        "[" ->
            Just ( "]", regex "[\\[\\]]", True )

        "]" ->
            Just ( "[", regex "[\\[\\]]", False )

        "{" ->
            Just ( "}", regex "[\\{\\}]", True )

        "}" ->
            Just ( "{", regex "[\\{\\}]", False )

        _ ->
            Nothing


pairBracket : Int -> Int -> TextBuffer -> Syntax -> Position -> Maybe Position
pairBracket top bottom lines syntax ( y, x ) =
    case
        lines
            |> B.getLine y
            |> Maybe.andThen (String.slice x (x + 1) >> bracket)
        --|> Debug.log "pairBracket"
    of
        Just ( toMatch, regexBrackets, forward ) ->
            let
                tokenType =
                    getToken ( y, x ) syntax
                        |> Maybe.map .tipe
                        |> Maybe.withDefault TokenOther

                -- (Position -> String -> Token -> a -> ( a, Bool ))
            in
                iterateTokens
                    forward
                    (\pos line token ( blance, _ ) ->
                        let
                            --_ =
                            --Debug.log "iterateTokens" ( forward, pos, line, token, blance )
                            ( y, x ) =
                                pos

                            matchBracket blance begin s =
                                case
                                    s
                                        |> Re.find (Re.AtMost 1) regexBrackets
                                        |> List.head
                                    --|> Debug.log "matchBracket"
                                of
                                    Just { match, index } ->
                                        let
                                            blance1 =
                                                if toMatch == match then
                                                    blance - 1
                                                else
                                                    blance + 1
                                        in
                                            if blance1 == 0 then
                                                ( ( blance1
                                                  , Just
                                                        ( y
                                                        , begin
                                                            + if forward then
                                                                index
                                                              else
                                                                -index - 1
                                                        )
                                                  )
                                                , True
                                                )
                                            else
                                                matchBracket blance1
                                                    (begin
                                                        + if forward then
                                                            index + 1
                                                          else
                                                            -index - 1
                                                    )
                                                    (String.dropLeft (index + 1) s)

                                    _ ->
                                        ( ( blance, Nothing ), False )
                        in
                            if tokenType == token.tipe then
                                line
                                    |> (if forward then
                                            String.slice x (x + token.length)
                                        else
                                            String.slice (x - token.length) x
                                                >> String.reverse
                                       )
                                    |> matchBracket blance x
                            else
                                ( ( blance, Nothing ), False )
                    )
                    lines
                    syntax
                    ( y
                    , if forward then
                        x + 1
                      else
                        x
                    )
                    (if forward then
                        bottom
                     else
                        top
                    )
                    ( 1, Nothing )
                    |> Tuple.second

        _ ->
            Nothing
