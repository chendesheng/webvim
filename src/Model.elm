module Model exposing
    ( Editor
    , Flags
    , Key
    , Model(..)
    , ServerArgs
    , cacheVimAST
    , cleanBuffers
    , createBuffer
    , getActiveBuffer
    , getBuffer
    , getLoadedBuffers
    , isTempBuffer
    , listBuffers
    , registerString
    , registerToString
    , registersDecoder
    , setBuffer
    , updateBuffer
    , updateGlobal
    , updateIme
    , updateWindow
    , windowDecoder
    , windowEncoder
    )

-- types referenced from Model should be here (expect internal types like Patch)
-- types only part of a message (like tokenize result) should be in
-- Model should not import Update.Message

import Boot
import Dict exposing (Dict)
import Helper.Helper exposing (filename)
import Ime exposing (IME)
import Internal.Jumps exposing (..)
import Internal.Position exposing (..)
import Internal.Syntax exposing (..)
import Internal.TextBuffer exposing (Patch(..))
import Internal.Window as Win exposing (Window)
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Buffer exposing (..)
import Model.BufferConfig exposing (..)
import Model.BufferHistory exposing (..)
import Model.Frame as Frame exposing (Frame, frameDecoder, frameEncoder)
import Model.Global exposing (..)
import Model.LoadBuffer exposing (..)
import Model.View exposing (..)
import Set
import Vim.AST as V exposing (VisualType(..))


type alias CodePoint =
    Int


type alias Flags =
    { service : String
    , buffers : Encode.Value
    , window : Encode.Value
    , registers : Encode.Value
    , cwd : String
    , exHistory : List String
    }


type alias ServerArgs =
    { pathSeperator : String
    , homedir : String
    }


createBuffer : String -> Global -> ( Global, Buffer )
createBuffer path global =
    let
        ( name, ext ) =
            filename path

        config =
            configs
                |> Dict.get ext
                |> Maybe.withDefault defaultBufferConfig
    in
    ( global
    , { emptyBuffer
        | id = path
        , config =
            { config
                | lint =
                    isLintEnabled global.pathSeperator
                        global.homedir
                        (name ++ ext)
                        config.lint
                , syntax = not <| isTempBuffer path
            }
        , path = path
        , name = name ++ ext
      }
    )


windowEncoder : Window Frame -> Encode.Value
windowEncoder =
    Win.windowEncoder frameEncoder


windowDecoder : Decode.Decoder (Window Frame)
windowDecoder =
    Win.windowDecoder frameDecoder


type alias Key =
    String


type Model
    = Booting Flags Boot.Model
    | Ready Global


type alias Editor =
    { buf : Buffer
    , global : Global
    }


isTempBuffer : String -> Bool
isTempBuffer path =
    String.isEmpty path || path == "[Search]"


getBuffer : String -> Dict String LoadBuffer -> Maybe Buffer
getBuffer id =
    Dict.get id
        >> Maybe.andThen getLoadedBuffer


getLoadedBuffers : Dict Int LoadBuffer -> List Buffer
getLoadedBuffers =
    Dict.values >> List.filterMap getLoadedBuffer


getActiveBuffer :
    { a | window : Window Frame, buffers : Dict String LoadBuffer }
    -> Maybe Buffer
getActiveBuffer { window, buffers } =
    Win.getActiveFrame window
        |> Maybe.andThen
            (\frame ->
                frame
                    |> Frame.getActiveViewId
                    |> Maybe.andThen (\bufId -> Dict.get bufId buffers)
                    |> Maybe.andThen getLoadedBuffer
            )


listBuffers : Dict String LoadBuffer -> List ( Buffer, Bool )
listBuffers =
    Dict.values
        >> List.map
            (\buf ->
                case buf of
                    NotLoad b ->
                        ( b, False )

                    Loaded b ->
                        ( b, True )
            )


setBuffer : Buffer -> Global -> Global
setBuffer buf global =
    { global
        | buffers =
            Dict.insert buf.id (Loaded buf) global.buffers
    }


cacheVimAST : ( String, String ) -> ( V.AST, String ) -> Global -> Global
cacheVimAST k v gb =
    { gb | vimASTCache = Dict.insert k v gb.vimASTCache }


updateIme : (IME -> IME) -> Global -> Global
updateIme fnupdate global =
    let
        ime =
            fnupdate global.ime
    in
    if ime == global.ime then
        global

    else
        { global | ime = ime }


updateBuffer : (Buffer -> Buffer) -> Editor -> Editor
updateBuffer fn ed =
    { ed | buf = fn ed.buf }


updateGlobal : (Global -> Global) -> Editor -> Editor
updateGlobal fn ed =
    { ed | global = fn ed.global }


updateWindow : (Window Frame -> Window Frame) -> Global -> Global
updateWindow fn global =
    { global | window = fn global.window }


registerString : RegisterText -> String
registerString reg =
    case reg of
        Text s ->
            s

        Lines s ->
            s


registerToString : Dict String RegisterText -> String
registerToString registers =
    registers
        |> Dict.toList
        |> Encode.list
            (\item ->
                let
                    ( k, v ) =
                        item
                in
                Encode.object
                    [ ( "name", Encode.string k )
                    , case v of
                        Text s ->
                            ( "type", Encode.string "text" )

                        Lines s ->
                            ( "type", Encode.string "lines" )
                    , case v of
                        Text s ->
                            ( "value", Encode.string s )

                        Lines s ->
                            ( "value", Encode.string s )
                    ]
            )
        |> Encode.encode 0


registersDecoder : Decode.Decoder (Dict String RegisterText)
registersDecoder =
    Decode.map3
        (\name tipe value ->
            ( name
            , case tipe of
                "text" ->
                    Text value

                _ ->
                    Lines value
            )
        )
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)
        (Decode.field "value" Decode.string)
        |> Decode.list
        |> Decode.map Dict.fromList


cleanBuffers : Global -> Global
cleanBuffers global =
    let
        bufIds =
            global.window
                |> Win.toList
                |> List.concatMap (\item -> List.map .bufId <| Frame.getViews item.frame)
                |> Set.fromList
    in
    { global
        | buffers =
            Dict.filter
                (\id b ->
                    (b
                        |> getBufferId
                        |> isTempBuffer
                    )
                        || Set.member id bufIds
                )
                global.buffers
    }
