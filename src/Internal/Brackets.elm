module Internal.Brackets exposing (..)

import Regex as Re exposing (Regex)
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
import Helper.Helper exposing (regex)


isBracket : Char -> Bool
isBracket c =
    List.any ((==) c)
        [ '(', ')', '[', ']', '{', '}', '<', '>' ]


bracketsParser : Parser Int
bracketsParser =
    P.succeed String.length
        |= (P.chompWhile (isBracket >> not)
                |> P.getChompedString
           )
        |. P.chompIf isBracket


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


pairBracketAt : Int -> Int -> TextBuffer -> Syntax -> Position -> Maybe Position
pairBracketAt top bottom lines syntax ( y, x ) =
    lines
        |> B.getLine y
        |> Maybe.map (String.slice x (x + 1))
        |> Maybe.andThen (pairBracket top bottom lines syntax ( y, x ))


pairBracket : Int -> Int -> TextBuffer -> Syntax -> Position -> String -> Maybe Position
pairBracket top bottom lines syntax ( y, x ) c =
    case bracket c of
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
                            ( y_, x_ ) =
                                pos

                            matchBracket blance_ begin s =
                                case
                                    s
                                        |> Re.findAtMost 1 regexBrackets
                                        |> List.head
                                    --|> Debug.log "matchBracket"
                                of
                                    Just { match, index } ->
                                        let
                                            blance1 =
                                                if toMatch == match then
                                                    blance_ - 1
                                                else
                                                    blance_ + 1
                                        in
                                            if blance1 == 0 then
                                                ( ( blance1
                                                  , Just
                                                        ( y_
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
                                            String.slice x_ (x_ + token.length)
                                        else
                                            String.slice (x_ - token.length) x_
                                                >> String.reverse
                                       )
                                    |> matchBracket blance x_
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
