module Boot exposing (Message, Model, Translator, init, update, view)

import Browser.Dom as Dom
import Font exposing (FontInfo, measureFont, renderMeasureDivs)
import Helper.Helper exposing (httpErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Url exposing (Url)
import Url.Parser as UrlParser
import Url.Parser.Query as Query



-- model


type Model
    = Booting BootArgs
    | BootFailed String


type alias BootArgs =
    { theme : String
    , serverArgs : Maybe ServerArgs
    , size : Maybe Size
    }


type alias Size =
    { width : Int, height : Int }


initBootArgs : String -> BootArgs
initBootArgs theme =
    { theme = theme
    , serverArgs = Nothing
    , size = Nothing
    }


type alias ServerArgs =
    { pathSeperator : String
    , homedir : String
    }


serverArgsDecoder : Decode.Decoder ServerArgs
serverArgsDecoder =
    Decode.map2
        (\homedir pathSeperator ->
            { pathSeperator = pathSeperator
            , homedir = homedir
            }
        )
        (Decode.field "homedir" Decode.string)
        (Decode.field "pathSeperator" Decode.string)



-- init


init : Translator msg model -> String -> Url -> ( model, Cmd msg )
init { toModel, toMsg } service url =
    ( url
        |> parseTheme
        |> initBootArgs
        |> Booting
        |> toModel
    , sendBoot service
        |> Cmd.map toMsg
    )


parseTheme : Url -> String
parseTheme =
    let
        withDefaultTheme =
            Maybe.withDefault "Solarized Dark"
    in
    (Query.string "theme"
        |> Query.map withDefaultTheme
        |> UrlParser.query
        |> UrlParser.parse
    )
        >> withDefaultTheme


sendBoot : String -> Cmd Message
sendBoot service =
    Http.get (service ++ "/boot") serverArgsDecoder
        |> Http.send
            (\result ->
                InitServerArgs <|
                    Result.mapError httpErrorMessage result
            )



-- view


view : Model -> Html msg
view model =
    case model of
        Booting _ ->
            renderMeasureDivs

        BootFailed message ->
            text <| "Boot failed: " ++ message



-- update


type Message
    = InitServerArgs (Result String ServerArgs)
    | InitSize { width : Float, height : Float }
    | InitFontInfo FontInfo


update :
    Translator msg model
    -> (String -> FontInfo -> Size -> ServerArgs -> ( model, Cmd msg ))
    -> Message
    -> Model
    -> ( model, Cmd msg )
update { toModel, toMsg } onReady msg model =
    case model of
        BootFailed _ ->
            ( toModel model, Cmd.none )

        Booting args ->
            case msg of
                InitServerArgs (Ok serverArgs) ->
                    ( toModel <| Booting { args | serverArgs = Just serverArgs }
                    , Task.perform (.scene >> InitSize >> toMsg) Dom.getViewport
                    )

                InitServerArgs (Err message) ->
                    ( toModel <| BootFailed message, Cmd.none )

                InitSize { width, height } ->
                    ( toModel <|
                        Booting
                            { args
                                | size =
                                    Just
                                        { width = round width
                                        , height = round height
                                        }
                            }
                    , measureFont InitFontInfo
                        |> Cmd.map toMsg
                    )

                InitFontInfo fontInfo ->
                    Maybe.map2
                        (onReady args.theme fontInfo)
                        args.size
                        args.serverArgs
                        |> Maybe.withDefault
                            ( toModel <| BootFailed "boot failed"
                            , Cmd.none
                            )



-- composition


type alias Translator msg model =
    { toMsg : Message -> msg
    , toModel : Model -> model
    }
