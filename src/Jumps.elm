module Jumps
    exposing
        ( saveCursorPosition
        , Location
        , Jumps
        , jump
        , jumpsToString
        )

import Position exposing (Position)


type alias Location =
    { path : String
    , cursor : Position
    }



-- backwards 1 -> 2 -> 3
-- current 0
-- forwards 1 -> 2 -> 3


type alias Jumps =
    { backwards : List Location
    , forwards : List Location
    , current : Location
    }


jumpsToString : Jumps -> String
jumpsToString { backwards, forwards, current } =
    let
        locationToString { path, cursor } =
            let
                ( y, x ) =
                    cursor
            in
                path ++ ":" ++ toString y ++ ":" ++ toString x

        joinStr s1 s2 =
            s1 ++ "\n\t\t↑\n" ++ s2

        backwardsToString backwards =
            List.foldl (\loc res -> joinStr res (locationToString loc))
                ""
                backwards

        forwardsToString backwards =
            List.foldl (\loc res -> joinStr (locationToString loc) res)
                ""
                forwards
    in
        backwards
            |> backwardsToString
            |> joinStr (locationToString current)
            |> joinStr (forwardsToString forwards)


saveCursorPosition : Location -> Jumps -> Jumps
saveCursorPosition loc { backwards, forwards, current } =
    { backwards = current :: List.foldl (::) backwards forwards
    , current = loc
    , forwards = []
    }


jumpForward : Jumps -> Jumps
jumpForward ({ backwards, forwards, current } as jumps) =
    case forwards of
        loc :: forwards2 ->
            { backwards = current :: backwards
            , current = loc
            , forwards = forwards2
            }

        _ ->
            jumps


jumpBackward : Jumps -> Jumps
jumpBackward ({ backwards, forwards, current } as jumps) =
    case backwards of
        loc :: backwards2 ->
            { backwards = backwards2
            , current = loc
            , forwards = current :: forwards
            }

        _ ->
            jumps


jump : Bool -> Jumps -> Jumps
jump isForward =
    if isForward then
        jumpForward
    else
        jumpBackward
