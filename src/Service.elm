module Service exposing (..)

import Model exposing (..)
import Http
import Message exposing (Msg(..))
import Internal.TextBuffer as B exposing (Patch(..))
import Json.Decode as Decode


sendEditBuffer : String -> String -> Cmd Msg
sendEditBuffer url path =
    url
        ++ "/edit?path="
        ++ path
        |> Http.getString
        |> Http.send
            (Result.map
                (\s ->
                    { path = path
                    , content = s
                    }
                )
                >> Read
            )


sendSaveBuffer : String -> String -> Buffer -> Cmd Msg
sendSaveBuffer url path buf =
    let
        body =
            buf.lines
                |> B.toString
                |> Http.stringBody "plain/text"
    in
        (Http.post
            (url ++ "/write?path=" ++ path)
            body
            (Decode.succeed ())
        )
            |> Http.send Write
