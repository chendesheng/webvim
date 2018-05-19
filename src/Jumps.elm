module Jumps
    exposing
        ( saveJump
        , Location
        , Jumps
        , jumpForward
        , jumpBackward
        , jumpsToString
        , currentLocation
        , applyPatchesToJumps
        , applyPatchesToLocations
        )

import Position exposing (Position)
import Internal.TextBuffer
    exposing
        ( Patch
        , shiftPositionByPatch
        )


type alias Location =
    { path : String
    , cursor : Position
    }



-- backwards 1 -> 2 -> 3
-- forwards 1 -> 2 -> 3


type alias Jumps =
    { backwards : List Location

    -- head of forwards is current location
    , forwards : List Location
    }


jumpsToString : Jumps -> String
jumpsToString { backwards, forwards } =
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
            |> joinStr (forwardsToString forwards)


sameLine : Location -> Location -> Bool
sameLine loc1 loc2 =
    (loc1.path == loc2.path)
        && (Tuple.first loc1.cursor == Tuple.first loc2.cursor)


saveJump : Location -> Jumps -> Jumps
saveJump loc { backwards, forwards } =
    { backwards =
        List.foldl (::) backwards forwards
            |> List.filter (sameLine loc >> not)
            |> ((::) loc)
    , forwards = []
    }


jumpForward : Jumps -> Jumps
jumpForward ({ backwards, forwards } as jumps) =
    case forwards of
        loc :: loc1 :: forwards2 ->
            { backwards = loc :: backwards
            , forwards = loc1 :: forwards2
            }

        _ ->
            jumps


jumpBackward : Location -> Jumps -> Jumps
jumpBackward cursor ({ backwards, forwards } as jumps) =
    case backwards of
        loc :: backwards2 ->
            if List.isEmpty forwards then
                { backwards =
                    List.filter (sameLine cursor >> not)
                        backwards2
                , forwards =
                    if sameLine loc cursor then
                        [ cursor ]
                    else
                        [ loc, cursor ]
                }
            else
                { backwards = backwards2
                , forwards = loc :: forwards
                }

        _ ->
            jumps


currentLocation : Jumps -> Maybe Location
currentLocation { forwards } =
    List.head forwards


applyPatchesToLocations : List Location -> List Patch -> List Location
applyPatchesToLocations locations patches =
    List.foldl
        (\patch result ->
            List.map
                (\loc ->
                    { loc
                        | cursor =
                            shiftPositionByPatch patch loc.cursor
                    }
                )
                result
        )
        locations
        patches


applyPatchesToJumps : List Patch -> Jumps -> Jumps
applyPatchesToJumps patches { backwards, forwards } =
    { backwards =
        applyPatchesToLocations backwards patches
    , forwards =
        applyPatchesToLocations forwards patches
    }
