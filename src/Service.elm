module Service exposing (..)

import Http
import Message
    exposing
        ( Msg(..)
        , BufferInfo
        , LocationItem
        , elmMakeResultDecoder
        , TokenizeResponse(..)
        )
import Json.Decode as Decode
import Elm.Array as Array
import Bitwise as Bit
import Syntax exposing (Token)
import List


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


isEven : Int -> Bool
isEven n =
    n % 2 == 0


tokensParser : Decode.Decoder (List Token)
tokensParser =
    Decode.list Decode.int
        |> Decode.map
            (\tokens ->
                let
                    ( indexes, classes ) =
                        tokens
                            |> List.map2 (,) (List.range 0 <| List.length tokens)
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
                        (indexes ++ [ -1 ])
                            |> List.tail
                            |> Maybe.withDefault []
                in
                    (List.map3
                        (\startIndex endIndex class ->
                            { length =
                                if endIndex == -1 then
                                    -1
                                else
                                    endIndex - startIndex
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
                        Decode.field "payload"
                            (Decode.list tokensParser
                                |> Decode.map
                                    (Array.fromList >> TokenizeSuccess n)
                            )

                    _ ->
                        Decode.succeed TokenizeCacheMiss
            )


sendTokenize : String -> Int -> String -> String -> Cmd Msg
sendTokenize url line path lines =
    let
        body =
            Http.stringBody "text/plain" lines
    in
        tokenizeResponseDecoder line
            |> Http.post
                (url
                    ++ "/tokenize?path="
                    ++ path
                    ++ "&line="
                    ++ (toString line)
                )
                body
            |> Http.send Tokenized
