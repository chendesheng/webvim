module Helper.Debounce exposing
    ( Message
    , Model
    , Translator
    , debounce
    , emptyModel
    , update
    )

import Process
import Task
import Time


type Model debouncedMsg
    = NotStart
    | Started debouncedMsg Time.Posix


emptyModel : Model debouncedMsg
emptyModel =
    NotStart


type Message debouncedMsg
    = Timeout Time.Posix
    | UpdateTime debouncedMsg Time.Posix


type alias Translator debouncedMsg model msg =
    { toModel : Model debouncedMsg -> model
    , toMsg : Message debouncedMsg -> msg
    , toMsgFromDebounced : debouncedMsg -> msg
    }


update :
    Translator debouncedMsg model msg
    -> (msg -> model -> ( model, Cmd msg ))
    -> Model debouncedMsg
    -> Message debouncedMsg
    -> ( model, Cmd msg )
update { toModel, toMsg, toMsgFromDebounced } onTimeout model msg =
    case msg of
        Timeout time ->
            case model of
                NotStart ->
                    ( toModel model, Cmd.none )

                Started debouncedMsg expectTime ->
                    let
                        dur =
                            duration time expectTime
                    in
                    if dur > 0 then
                        -- not reach expectTime yet, keep waiting
                        ( toModel model
                        , toFloat dur
                            |> Process.sleep
                            |> Task.perform (always <| Timeout expectTime)
                            |> Cmd.map toMsg
                        )

                    else
                        onTimeout (toMsgFromDebounced debouncedMsg) (toModel NotStart)

        UpdateTime debouncedMsg t ->
            ( toModel <| Started debouncedMsg t, Cmd.none )


duration : Time.Posix -> Time.Posix -> Int
duration a b =
    Time.posixToMillis b - Time.posixToMillis a


debounce : Model msg -> Int -> debouncedMsg -> Cmd (Message debouncedMsg)
debounce model millis msg =
    Cmd.batch
        [ Time.now
            |> Task.map
                (\time ->
                    Time.posixToMillis time
                        + millis
                        |> Time.millisToPosix
                )
            |> Task.perform (UpdateTime msg)
        , case model of
            NotStart ->
                Process.sleep (toFloat millis)
                    |> Task.andThen (always Time.now)
                    |> Task.perform Timeout

            Started _ _ ->
                Cmd.none
        ]
