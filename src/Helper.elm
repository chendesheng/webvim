module Helper exposing (..)


getLast : List a -> Maybe a
getLast xs =
    case xs of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            getLast xs


minMaybe : Maybe Int -> Maybe Int -> Maybe Int
minMaybe a b =
    if a == Nothing then
        b
    else if b == Nothing then
        a
    else
        Maybe.map2 (\x y -> Basics.min x y) a b
