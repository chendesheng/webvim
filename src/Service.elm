module Service exposing (..)

import Http
import Message exposing (Msg(..), BufferInfo)
import Json.Decode as Decode


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


sendSaveBuffer : String -> String -> String -> Cmd Msg
sendSaveBuffer url path buf =
    let
        body =
            Http.stringBody "text/plain" buf
    in
        (Http.post
            (url ++ "/write?path=" ++ path)
            body
            (Decode.succeed ())
        )
            |> Http.send Write
