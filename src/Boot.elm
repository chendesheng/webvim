module Boot exposing (Message, Model, Translator, init, update, view)

import Browser.Dom as Dom
import Font exposing (FontInfo, measureFont, renderMeasureDivs)
import Helper.Helper exposing (httpErrorMessage)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Http
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes as SvgAttr
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
    { width : Int
    , height : Int
    }


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
    Http.get
        { url = service ++ "/boot"
        , expect =
            Http.expectJson
                (\result ->
                    InitServerArgs <|
                        Result.mapError httpErrorMessage result
                )
                serverArgsDecoder
        }



-- view


forbidden =
    g []
        [ circle
            [ SvgAttr.cx "100"
            , SvgAttr.cy "100"
            , SvgAttr.r "60"
            , SvgAttr.style "fill:none;stroke:darkred;stroke-width:20"
            , SvgAttr.strokeOpacity "0.5"
            ]
            []
        , line
            [ SvgAttr.x1 "50"
            , SvgAttr.y1 "100"
            , SvgAttr.x2 "150"
            , SvgAttr.y2 "100"
            , SvgAttr.style "fill:none;stroke:darkred;stroke-width:20;"
            , SvgAttr.transform "rotate(45, 100, 100)"
            , SvgAttr.strokeOpacity "0.5"
            ]
            []
        ]


logo title animation failed =
    div
        [ class "logo"
        , classList [ ( "logo-initializing", animation ) ]
        , HtmlAttr.title title
        ]
        [ svg [ height 200, width 200 ]
            ([ polygon [ SvgAttr.points "0,0 0,100 100,100", SvgAttr.style "fill:rgb(127,178,198);" ] []
             , polygon [ SvgAttr.points "0.5,0 0.5,100 100,100", SvgAttr.style "fill:none;stroke:black;" ] []
             , polygon [ SvgAttr.points "200,0 100,100 200,100", SvgAttr.style "fill:rgb(92,99,116);" ] []
             , polygon [ SvgAttr.points "199.5,0 100,100 199.5,100", SvgAttr.style "fill:none;stroke:black;" ] []
             , polygon [ SvgAttr.points "0,100 50,100 50,150", SvgAttr.style "fill:rgb(225,175,83);stroke:black;" ] []
             , polygon [ SvgAttr.points "50,100 100,100 100,150 50,150", SvgAttr.style "fill:rgb(58,202,101);stroke:black;" ] []
             , polygon [ SvgAttr.points "100,100 150,100 100,150", SvgAttr.style "fill:rgb(225,175,83);stroke:black;" ] []
             , polygon [ SvgAttr.points "150,100 100,150 150,150 200,100", SvgAttr.style "fill:rgb(58,202,101);stroke:black;" ] []
             , polygon [ SvgAttr.points "50,150 150,150 100,200", SvgAttr.style "fill:rgb(127,178,198);stroke:black;" ] []
             ]
                ++ (if failed then
                        [ forbidden ]

                    else
                        []
                   )
            )
        ]


view : Model -> Html msg
view model =
    case model of
        Booting _ ->
            div []
                [ renderMeasureDivs, logo "Initializing" True False ]

        BootFailed message ->
            div []
                [ logo message False True ]



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
