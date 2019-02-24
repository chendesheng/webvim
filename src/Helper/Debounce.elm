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


type Model debMsg
    = NotStart
    | Started debMsg Time.Posix


emptyModel : Model debMsg
emptyModel =
    NotStart


type Message debMsg
    = Timeout Time.Posix
    | UpdateTime debMsg Time.Posix


{-| There are two kinds of messages here,
one is for debounce related actions (Message debMsg),
the other one is the carried message which will send when timeout (debMsg)
-}
type alias Translator debMsg model msg =
    { toModel : Model debMsg -> model
    , toMsg : Message debMsg -> msg
    , toMsgFromDebounced : debMsg -> msg
    }


update :
    Translator debMsg model msg
    -> (msg -> model -> ( model, Cmd msg ))
    -> Model debMsg
    -> Message debMsg
    -> ( model, Cmd msg )
update { toModel, toMsg, toMsgFromDebounced } onTimeout model msg =
    case msg of
        Timeout time ->
            case model of
                NotStart ->
                    ( toModel model, Cmd.none )

                Started debMsg expectTime ->
                    let
                        dur =
                            duration time expectTime
                    in
                    if dur > 0 then
                        -- not reach expectTime yet, keep waiting
                        ( toModel model
                        , toFloat dur
                            |> Process.sleep
                            -- when sleep end, consider expectTime as current time
                            |> Task.perform (always <| Timeout expectTime)
                            |> Cmd.map toMsg
                        )

                    else
                        onTimeout (toMsgFromDebounced debMsg) (toModel NotStart)

        UpdateTime debMsg t ->
            ( toModel <| Started debMsg t, Cmd.none )


duration : Time.Posix -> Time.Posix -> Int
duration from to =
    Time.posixToMillis to - Time.posixToMillis from


debounce : Model debMsg -> Int -> debMsg -> ( Model debMsg, Cmd (Message debMsg) )
debounce model millis debMsg =
    case model of
        NotStart ->
            -- this will make sure we don't sleep multiple times
            -- use 0 ensures it will smaller than current time, so
            -- unless we send UpdateTime before sleep end, it will always timeout
            ( Started debMsg (Time.millisToPosix 0)
            , Process.sleep (toFloat millis)
                |> Task.andThen (always Time.now)
                |> Task.perform Timeout
            )

        Started _ _ ->
            ( model
            , Time.now
                |> Task.map
                    (\time ->
                        Time.posixToMillis time + millis |> Time.millisToPosix
                    )
                |> Task.perform (UpdateTime debMsg)
            )
