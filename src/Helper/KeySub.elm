effect module Helper.KeySub where { subscription = MySub } exposing (..)

import Helper.KeyEvent exposing (decodeKeyboardEvent)
import Dict
import Process
import Task exposing (Task)
import Dict
import Platform
import Json.Decode as Decode
import Native.Doc


{-| Add an event handler on the `document`. The resulting task will never end,
and when you kill the process it is on, it will detach the relevant JavaScript
event listener.
-}
onDocument :
    String
    -> Decode.Decoder msg
    -> (msg -> Task Never ())
    -> Task Never Never
onDocument =
    Native.Doc.onDocument


type alias Key =
    String


{-| Subscribe to all key presses.
-}
presses : (Key -> msg) -> Sub msg
presses tagger =
    subscription (MySub "keypress" tagger)


{-| Subscribe to get codes whenever a key goes down.
-}
downs : (Key -> msg) -> Sub msg
downs tagger =
    subscription (MySub "keydown" tagger)


{-| Subscribe to get codes whenever a key goes up.
-}
ups : (Key -> msg) -> Sub msg
ups tagger =
    subscription (MySub "keyup" tagger)



-- SUBSCRIPTIONS


type MySub msg
    = MySub String (Key -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
    MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
    Dict.Dict String (Watcher msg)


type alias Watcher msg =
    { taggers : List (Key -> msg)
    , pid : Process.Id
    }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
    Dict.Dict String (List (Key -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
    categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
    case subs of
        [] ->
            subDict

        (MySub category tagger) :: rest ->
            categorizeHelp rest <|
                Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
    case maybeValues of
        Nothing ->
            Just [ value ]

        Just values ->
            Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { category : String
    , event : Key
    }


(&>) : Task x a -> Task x b -> Task x b
(&>) task1 task2 =
    Task.andThen (\_ -> task2) task1


onEffects :
    Platform.Router msg Msg
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep category { pid } task =
            Process.kill pid &> task

        bothStep category { pid } taggers task =
            Task.map (Dict.insert category (Watcher taggers pid)) task

        rightStep category taggers task =
            task
                |> Task.andThen
                    (\state ->
                        Process.spawn
                            (onDocument
                                category
                                decodeKeyboardEvent
                                (Platform.sendToSelf router << Msg category)
                            )
                            |> Task.andThen
                                (\pid ->
                                    Task.succeed
                                        (Dict.insert
                                            category
                                            (Watcher taggers pid)
                                            state
                                        )
                                )
                    )
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (categorize newSubs)
            (Task.succeed Dict.empty)


onSelfMsg :
    Platform.Router msg Msg
    -> Msg
    -> State msg
    -> Task Never (State msg)
onSelfMsg router { category, event } state =
    if event == "" then
        Task.succeed state
    else
        case Dict.get category state of
            Nothing ->
                Task.succeed state

            Just { taggers } ->
                let
                    send tagger =
                        Platform.sendToApp router (tagger event)
                in
                    Task.sequence (List.map send taggers)
                        |> Task.andThen (\_ -> Task.succeed state)
