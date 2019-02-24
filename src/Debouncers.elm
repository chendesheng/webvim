module Debouncers exposing
    ( DebounceMessage(..)
    , Debouncers
    , debounceLint
    , debouncePersistentAll
    , debounceTokenize
    , debouncerUpdate
    , emptyDebouncers
    )

import Dict
import Helper.Debounce as Deb



-- model


type DebounceMessage
    = SendLint
    | SendTokenize
    | PersistentAll


debounceMessageToInt : DebounceMessage -> Int
debounceMessageToInt m =
    case m of
        SendLint ->
            0

        SendTokenize ->
            1

        PersistentAll ->
            2


type Debouncers
    = Debouncers (Dict.Dict Int (Deb.Model DebounceMessage))


emptyDebouncers : Debouncers
emptyDebouncers =
    Debouncers Dict.empty


dictGetOrAdd :
    comparable
    -> Dict.Dict comparable value
    -> value
    -> ( value, Dict.Dict comparable value )
dictGetOrAdd key dict newValue =
    case Dict.get key dict of
        Just value ->
            ( value, dict )

        Nothing ->
            ( newValue, Dict.insert key newValue dict )


getDebouncer :
    DebounceMessage
    -> Dict.Dict Int (Deb.Model DebounceMessage)
    -> Deb.Model DebounceMessage
getDebouncer debMsg debouncers =
    Dict.get (debounceMessageToInt debMsg) debouncers
        |> Maybe.withDefault Deb.emptyModel



-- send debounce message


debounce :
    DebounceMessage
    -> (DebounceMessage -> Deb.Message DebounceMessage -> msg)
    -> Debouncers
    -> Int
    -> ( Debouncers, Cmd msg )
debounce msg toMsg (Debouncers debouncers) delay =
    let
        key =
            debounceMessageToInt msg

        ( debouncer, debouncers1 ) =
            dictGetOrAdd key debouncers Deb.emptyModel

        ( debouncer1, cmd ) =
            Deb.debounce debouncer delay msg
    in
    ( Debouncers (Dict.insert key debouncer1 debouncers1)
    , Cmd.map (toMsg msg) cmd
    )


debounceLint :
    (DebounceMessage -> Deb.Message DebounceMessage -> msg)
    -> Debouncers
    -> Int
    -> ( Debouncers, Cmd msg )
debounceLint =
    debounce SendLint


debounceTokenize :
    (DebounceMessage -> Deb.Message DebounceMessage -> msg)
    -> Debouncers
    -> Int
    -> ( Debouncers, Cmd msg )
debounceTokenize =
    debounce SendTokenize


debouncePersistentAll :
    (DebounceMessage -> Deb.Message DebounceMessage -> msg)
    -> Debouncers
    -> Int
    -> ( Debouncers, Cmd msg )
debouncePersistentAll =
    debounce PersistentAll



-- update


debouncerUpdate :
    { toMsg : DebounceMessage -> Deb.Message DebounceMessage -> msg
    , toModel : Debouncers -> model
    , toMsgFromDebounced : DebounceMessage -> msg
    }
    -> (msg -> model -> ( model, Cmd msg ))
    -> DebounceMessage
    -> Deb.Message DebounceMessage
    -> Debouncers
    -> ( model, Cmd msg )
debouncerUpdate translator onTimeout debMsg payload (Debouncers debouncers) =
    let
        { toMsg, toModel, toMsgFromDebounced } =
            translator
    in
    Deb.update
        { toMsg = toMsg debMsg
        , toModel =
            \m ->
                debouncers
                    |> Dict.insert (debounceMessageToInt debMsg) m
                    |> Debouncers
                    |> toModel
        , toMsgFromDebounced = toMsgFromDebounced
        }
        onTimeout
        (getDebouncer debMsg debouncers)
        payload
