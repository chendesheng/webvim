module Zipper exposing
    ( Zipper
    , empty
    , filter
    , foldl
    , fromList
    , getBackwards
    , getCurrent
    , getForwards
    , getNth
    , insert
    , insertBackward
    , isBackwardsEmpty
    , isEmpty
    , isForwardsEmpty
    , map
    , moveBackward
    , moveForward
    , moveToEnd
    , moveToHead
    , remove
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


insertBackward : a -> Zipper a -> Zipper a
insertBackward item (Zipper left right) =
    Zipper (item :: left) right


insert : a -> Zipper a -> Zipper a
insert item (Zipper left right) =
    Zipper left (item :: right)


map : (a -> b) -> Zipper a -> Zipper b
map update (Zipper left right) =
    Zipper (List.map update left) (List.map update right)


filter : (a -> Bool) -> Zipper a -> Zipper a
filter f (Zipper left right) =
    Zipper (List.filter f left) (List.filter f right)


foldl : (a -> b -> b) -> b -> Zipper a -> b
foldl fn item zipper =
    case moveToHead zipper of
        Zipper _ right ->
            List.foldl fn item right


moveToHead : Zipper a -> Zipper a
moveToHead (Zipper left right) =
    Zipper [] (List.foldl (::) right left)


moveToEnd : Zipper a -> Zipper a
moveToEnd (Zipper left right) =
    Zipper (List.foldl (::) left right) []


remove : Zipper a -> Zipper a
remove (Zipper left right) =
    Zipper left (List.tail right |> Maybe.withDefault [])


fromList : List a -> Zipper a
fromList right =
    Zipper [] right


toList : Zipper a -> List a
toList zipper =
    case moveToHead zipper of
        Zipper _ right ->
            right


find : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find pred zipper =
    findHelper pred (moveToHead zipper)


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
