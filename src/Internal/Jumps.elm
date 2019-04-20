module Internal.Jumps exposing
    ( Jumps
    , Location
    , applyPatchesToJumps
    , applyPatchesToLocations
    , currentLocation
    , jumpBackward
    , jumpForward
    , saveJump
    )

import Internal.Position exposing (Position)
import Internal.TextBuffer
    exposing
        ( Patch
        , RegionChange
        , shiftPositionByRegionChange
        )
import String exposing (fromInt)
import Zipper exposing (Zipper)


type alias Location =
    { path : String
    , cursor : Position
    }



-- backwards 1 -> 2 -> 3
-- forwards 1 -> 2 -> 3


type alias Jumps =
    Zipper Location


sameLine : Location -> Location -> Bool
sameLine loc1 loc2 =
    (loc1.path == loc2.path)
        && (Tuple.first loc1.cursor == Tuple.first loc2.cursor)


saveJump : Location -> Jumps -> Jumps
saveJump loc jumps =
    jumps
        |> Zipper.filter (sameLine loc >> not)
        |> Zipper.moveToEnd
        |> Zipper.insertBackward loc


jumpForward : Jumps -> Jumps
jumpForward jumps =
    jumps
        |> Zipper.moveForward
        |> Maybe.withDefault jumps


jumpBackward : Location -> Jumps -> Jumps
jumpBackward cursor jumps =
    if Zipper.isForwardsEmpty jumps then
        jumps
            |> Zipper.filter (sameLine cursor >> not)
            |> Zipper.insert cursor
            |> Zipper.moveBackward
            |> Maybe.withDefault jumps

    else
        jumps
            |> Zipper.moveBackward
            |> Maybe.withDefault jumps


currentLocation : Jumps -> Maybe Location
currentLocation =
    Zipper.getCurrent


applyPatchesToLocations : String -> List RegionChange -> List Location -> List Location
applyPatchesToLocations path changes =
    List.map (applyPatchesToLocation path changes)


applyPatchesToLocation : String -> List RegionChange -> Location -> Location
applyPatchesToLocation path changes location =
    List.foldl
        (\change loc ->
            if path == loc.path then
                { loc
                    | cursor =
                        shiftPositionByRegionChange change loc.cursor
                }

            else
                loc
        )
        location
        changes


applyPatchesToJumps : String -> List RegionChange -> Jumps -> Jumps
applyPatchesToJumps path diff =
    Zipper.map (applyPatchesToLocation path diff)
