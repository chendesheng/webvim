module HistoryTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import History exposing (StateHistory, commit)
import Test exposing (..)


type Patch
    = Increment Int
    | Decrement Int


type alias State =
    StateHistory Int Patch


mergePatch : Patch -> Patch -> List Patch
mergePatch a b =
    [ case a of
        Increment x ->
            case b of
                Increment y ->
                    Increment <| x + y

                Decrement y ->
                    if x - y >= 0 then
                        Increment <| x - y

                    else
                        Decrement <| y - x

        Decrement x ->
            case b of
                Increment y ->
                    if -x + y >= 0 then
                        Increment <| -x + y

                    else
                        Decrement <| x - y

                Decrement y ->
                    Decrement <| x + y
    ]


historySample : State
historySample =
    History.empty 0
        |> exec (Increment 1)
        |> exec (Increment 1)
        |> exec (Increment 1)
        |> commit
        |> exec (Increment 1)
        |> exec (Increment 1)
        |> commit


applyPatch : Patch -> Int -> ( Int, Patch )
applyPatch patch state =
    case patch of
        Increment n ->
            ( state + n, Decrement n )

        Decrement n ->
            ( state - n, Increment n )


undo : State -> State
undo =
    History.undo applyPatch


redo : State -> State
redo =
    History.redo applyPatch


exec : Patch -> State -> State
exec =
    History.exec applyPatch mergePatch


suite : Test
suite =
    describe "History"
        [ test "exec" <|
            \_ ->
                let
                    state =
                        History.empty 0
                            |> exec (Increment 1)
                            |> exec (Increment 1)
                            |> exec (Decrement 1)
                in
                Expect.equal
                    { undoes = []
                    , redoes = []
                    , pending = [ Decrement 1 ]
                    , savePoint = 0
                    }
                <|
                    History.getHistory state
        , test "commit" <|
            \_ ->
                historySample
                    |> History.getHistory
                    |> Expect.equal
                        { undoes = [ [ Decrement 2 ], [ Decrement 3 ] ]
                        , redoes = []
                        , pending = []
                        , savePoint = 2
                        }
        , test "undo" <|
            Expect.all
                [ \_ ->
                    historySample
                        |> undo
                        |> History.getState
                        |> Expect.equal 3
                , \_ ->
                    historySample
                        |> undo
                        |> History.getHistory
                        |> Expect.equal
                            { undoes = [ [ Decrement 3 ] ]
                            , redoes = [ [ Increment 2 ] ]
                            , pending = []
                            , savePoint = 1
                            }
                ]
        , test "redo" <|
            \_ ->
                historySample
                    |> undo
                    |> redo
                    |> Expect.equal historySample
        ]
