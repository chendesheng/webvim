module Zipper exposing
    ( Zipper
    , empty
    , foldl
    , fromList
    , getBackwards
    , getCurrent
    , getForwards
    , getNth
    , insert
    , isBackwardsEmpty
    , isEmpty
    , isForwardsEmpty
    , map
    , moveBackward
    , moveForward
    , remove
    , rewind
    , toList
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Zipper a
    = Zipper (List a) (List a)


empty : Zipper a
empty =
    Zipper [] []


getCurrent : Zipper a -> Maybe a
getCurrent (Zipper _ right) =
    List.head right


isEmpty : Zipper a -> Bool
isEmpty (Zipper left right) =
    List.isEmpty left && List.isEmpty right


isBackwardsEmpty : Zipper a -> Bool
isBackwardsEmpty (Zipper left _) =
    List.isEmpty left


isForwardsEmpty : Zipper a -> Bool
isForwardsEmpty (Zipper _ right) =
    List.isEmpty right


moveBackward : Zipper a -> Maybe (Zipper a)
moveBackward ((Zipper left right) as zipper) =
    case left of
        item :: rest ->
            Just <| Zipper rest (item :: right)

        _ ->
            Nothing


moveForward : Zipper a -> Maybe (Zipper a)
moveForward ((Zipper left right) as zipper) =
    case right of
        item :: ((_ :: _) as rest) ->
            Just <| Zipper (item :: left) rest

        _ ->
            Nothing


insert : a -> Zipper a -> Zipper a
insert item (Zipper left right) =
    Zipper left (item :: right)


map : (a -> b) -> Zipper a -> Zipper b
map update (Zipper left right) =
    Zipper (List.map update left) (List.map update right)


foldl : (a -> b -> b) -> b -> Zipper a -> b
foldl fn item zipper =
    case rewind zipper of
        Zipper _ right ->
            List.foldl fn item right


rewind : Zipper a -> Zipper a
rewind (Zipper left right) =
    Zipper [] (List.foldl (::) right left)


remove : Zipper a -> Zipper a
remove (Zipper left right) =
    Zipper left (List.tail right |> Maybe.withDefault [])


fromList : List a -> Zipper a
fromList right =
    Zipper [] right


toList : Zipper a -> List a
toList zipper =
    case rewind zipper of
        Zipper _ right ->
            right


find : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find pred zipper =
    findHelper pred (rewind zipper)


findHelper : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findHelper pred zipper =
    case getCurrent zipper of
        Just x ->
            if pred x then
                Just zipper

            else
                moveForward zipper
                    |> Maybe.andThen (findHelper pred)

        _ ->
            Nothing


getBackwards : Zipper a -> List a
getBackwards (Zipper left _) =
    left


getForwards : Zipper a -> List a
getForwards (Zipper _ right) =
    right


getNth : Int -> Zipper a -> Maybe a
getNth n zipper =
    if n == 0 then
        getCurrent zipper

    else
        case moveForward zipper of
            Just z ->
                getNth (n - 1) z

            Nothing ->
                Nothing
