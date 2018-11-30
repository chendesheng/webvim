module Internal.Jumps
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

import Internal.Position exposing (Position)
import Internal.TextBuffer
    exposing
        ( Patch
        , shiftPositionByRegionChange
        , RegionChange
        )
import String exposing (fromInt)


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
                path ++ ":" ++ fromInt y ++ ":" ++ fromInt x

        joinStr s1 s2 =
            s1 ++ "\n\t\t↑\n" ++ s2

        backwardsToString backwards_ =
            List.foldl (\loc res -> joinStr res (locationToString loc))
                ""
                backwards_

        forwardsToString forwards_ =
            List.foldl (\loc res -> joinStr (locationToString loc) res)
                ""
                forwards_
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


applyPatchesToLocations : List Location -> List RegionChange -> List Location
applyPatchesToLocations locations changes =
    let
        _ =
            Debug.log "applyPatchesToLocations" ( locations, changes )
    in
        (List.foldl
            (\change result ->
                List.map
                    (\loc ->
                        { loc
                            | cursor =
                                shiftPositionByRegionChange change loc.cursor
                        }
                    )
                    result
            )
            locations
            changes
        )
            |> Debug.log "result"


applyPatchesToJumps : List RegionChange -> Jumps -> Jumps
applyPatchesToJumps diff { backwards, forwards } =
    { backwards =
        applyPatchesToLocations backwards diff
    , forwards =
        applyPatchesToLocations forwards diff
    }
