module Message exposing (..)

import Window exposing (Size)
import Result
import Http
import Position exposing (Position)
import Json.Decode as Decode
import Json.Encode as Encode
import Syntax exposing (Token, Syntax)


type alias File =
    String


type alias Key =
    String


type alias BufferInfo =
    { path : String
    , cursor : Position
    , scrollTop : Int
    , content : Maybe String
    }


bufferInfoEncoder : BufferInfo -> Encode.Value
bufferInfoEncoder info =
    [ ( "path", Encode.string info.path )
    , ( "cursor"
      , Encode.list
            [ info.cursor |> Tuple.first |> Encode.int
            , info.cursor |> Tuple.second |> Encode.int
            ]
      )
    , ( "scrollTop", Encode.int info.scrollTop )
    ]
        |> Encode.object


bufferInfoToString : BufferInfo -> String
bufferInfoToString info =
    info
        |> bufferInfoEncoder
        |> Encode.encode 0


buffersInfoToString : List BufferInfo -> String
buffersInfoToString buffers =
    buffers
        |> List.map bufferInfoEncoder
        |> Encode.list
        |> Encode.encode 0


bufferInfoDecoder : Decode.Decoder BufferInfo
bufferInfoDecoder =
    Decode.map3
        (\path scrollTop cursor ->
            { path = path
            , scrollTop = scrollTop
            , cursor = cursor
            , content = Nothing
            }
        )
        (Decode.field "path" Decode.string)
        (Decode.field "scrollTop" Decode.int)
        (Decode.field "cursor" (Decode.list Decode.int)
            |> Decode.map
                (\xs ->
                    case xs of
                        a :: b :: _ ->
                            ( a, b )

                        _ ->
                            ( 0, 0 )
                )
        )


type alias LocationItem =
    { tipe : String
    , tag : String
    , file : String
    , overview : String
    , details : String
    , region : ( Position, Position )
    , subRegion : Maybe ( Position, Position )
    }


positionDecoder : Decode.Decoder Position
positionDecoder =
    Decode.map2 (\a b -> ( a - 1, b - 1 ))
        (Decode.field "line" Decode.int)
        (Decode.field "column" Decode.int)


regionDecoder : Decode.Decoder ( Position, Position )
regionDecoder =
    Decode.map2 (,)
        (Decode.field "start" positionDecoder)
        (Decode.field "end" positionDecoder)


elmMakeResultDecoder : Decode.Decoder (List LocationItem)
elmMakeResultDecoder =
    Decode.map7 LocationItem
        (Decode.field "type" Decode.string)
        (Decode.field "tag" Decode.string)
        (Decode.field "file" Decode.string)
        (Decode.field "overview" Decode.string)
        (Decode.field "details" Decode.string)
        (Decode.field "region" regionDecoder)
        (Decode.field "subregion" regionDecoder |> Decode.maybe)
        |> Decode.list


type TokenizeResponse
    = TokenizeSuccess String Int Int Syntax
    | LineTokenizeSuccess String Int Int (List Token)
    | TokenizeCacheMiss -- happens when server restart


type alias TokenizeRequest =
    { path : String
    , version : Int
    , line : Int
    , lines : String
    }


type Msg
    = PressKey Key -- buffer id, key
    | Resize Size
    | Read (Result Http.Error BufferInfo)
    | Write (Result Http.Error String)
    | Edit BufferInfo
    | SendLint
    | SendTokenize TokenizeRequest
    | Lint (Result String (List LocationItem))
    | LintOnTheFly (Result String (List LocationItem))
    | Tokenized (Result Http.Error TokenizeResponse)
    | ListFiles (Result String (List File))
    | NoneMessage
