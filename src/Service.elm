module Service exposing (..)

import Http
import Message
    exposing
        ( Msg(..)
        , BufferInfo
        , LocationItem
        , elmMakeResultDecoder
        )


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


sendLintProject : String -> Cmd Msg
sendLintProject url =
    elmMakeResultDecoder
        |> Http.get (url ++ "/lint")
        |> Http.send Lint


sendLintOnTheFly : String -> String -> String -> Cmd Msg
sendLintOnTheFly url path buf =
    let
        body =
            Http.stringBody "text/plain" buf
    in
        elmMakeResultDecoder
            |> Http.post (url ++ "/lint?path=" ++ path) body
            |> Http.send LintOnTheFly
