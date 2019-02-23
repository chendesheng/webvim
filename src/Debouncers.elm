module Debouncers exposing
    ( DebounceMessage(..)
    , Debouncers
    , debounceLint
    , debouncePersistentAll
    , debounceTokenize
    , emptyDebouncers
    )

import Helper.Debounce as Deb


type DebounceMessage
    = SendLint
    | SendTokenize
    | PersistentAll


type alias Debouncers =
    { lint : Deb.Model DebounceMessage
    , persistentAll : Deb.Model DebounceMessage
    , tokenize : Deb.Model DebounceMessage
    }


emptyDebouncers : Debouncers
emptyDebouncers =
    { lint = Deb.emptyModel
    , persistentAll = Deb.emptyModel
    , tokenize = Deb.emptyModel
    }


debounceLint :
    (DebounceMessage -> Deb.Message DebounceMessage -> msg)
    -> Debouncers
    -> Int
    -> Cmd msg
debounceLint toMsg debouncers delay =
    Deb.debounce debouncers.lint delay SendLint
        |> Cmd.map (toMsg SendLint)


debounceTokenize :
    (DebounceMessage -> Deb.Message DebounceMessage -> msg)
    -> Debouncers
    -> Int
    -> Cmd msg
debounceTokenize toMsg debouncers delay =
    Deb.debounce debouncers.tokenize delay SendTokenize
        |> Cmd.map (toMsg SendTokenize)


debouncePersistentAll :
    (DebounceMessage -> Deb.Message DebounceMessage -> msg)
    -> Debouncers
    -> Int
    -> Cmd msg
debouncePersistentAll toMsg debouncers delay =
    Deb.debounce debouncers.persistentAll delay PersistentAll
        |> Cmd.map (toMsg PersistentAll)
