module Update.CTag exposing (onReadTags, startJumpToTag)

import Helper.Helper exposing (..)
import Http
import Internal.Jumps exposing (..)
import Model exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Update.Buffer as Buf
import Update.Jump exposing (..)
import Update.Message exposing (..)
import Update.Motion exposing (wordStringUnderCursor)


pickLocation : Int -> List Location -> Maybe Location
pickLocation index locs =
    locs
        |> List.sortBy (\{ path } -> ( String.length path, path ))
        |> nthList index


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


sendReadTags : String -> String -> String -> String -> Int -> String -> Cmd Msg
sendReadTags url sep cwd path index name =
    Http.get
        { url = url ++ "/readtags?name=" ++ name ++ "&cwd=" ++ cwd
        , expect =
            Http.expectString
                (\result ->
                    result
                        |> Result.mapError httpErrorMessage
                        |> Result.andThen
                            (\s ->
                                P.run (ctagsParser sep) s
                                    --|> Debug.log "ctags parse"
                                    |> Result.mapError
                                        (always "parse result error")
                                    |> Result.andThen
                                        (\locs ->
                                            locs
                                                |> pickLocation index
                                                |> Result.fromMaybe
                                                    "parse result error"
                                        )
                            )
                        |> ReadTags
                )
        }


startJumpToTag : Maybe Int -> Editor -> ( Editor, Cmd Msg )
startJumpToTag count ({ buf, global } as ed) =
    case
        wordStringUnderCursor
            buf.config.wordChars
            buf.lines
            buf.view.cursor
    of
        Just ( _, s ) ->
            ( ed
            , sendReadTags global.service
                global.pathSeperator
                global.cwd
                buf.path
                (Maybe.withDefault 1 count - 1)
                s
            )

        _ ->
            ( ed, Cmd.none )


onReadTags : Result String Location -> Editor -> ( Editor, Cmd Msg )
onReadTags result ed =
    case result of
        Ok loc ->
            let
                saveLastJumpToTag ed_ =
                    let
                        global_ =
                            ed_.global

                        buf_ =
                            ed_.buf

                        last =
                            global_.last
                    in
                    { ed_
                        | global =
                            { global_
                                | last =
                                    { last
                                        | jumpToTag =
                                            Just
                                                { path = buf_.path
                                                , cursor = buf_.view.cursor
                                                }
                                    }
                            }
                    }
            in
            ed
                |> updateBuffer Buf.clearMessage
                |> saveLastJumpToTag
                |> jumpToLocation True loc

        _ ->
            ( updateBuffer (Buf.errorMessage "tag not found") ed
            , Cmd.none
            )
