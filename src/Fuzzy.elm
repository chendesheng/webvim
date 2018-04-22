module Fuzzy exposing (..)

import List
import String
import Helper exposing (getLast)


type alias FuzzyMatchItem =
    { text : String
    , matches : List Int
    }


fuzzyMatchInner : String -> String -> List Int
fuzzyMatchInner s t =
    let
        charAt i =
            String.slice i (i + 1)
                >> String.uncons
                >> Maybe.map Tuple.first

        indexOf i s c =
            case String.uncons s of
                Just ( c1, rest ) ->
                    if c1 == c then
                        Just i
                    else
                        indexOf (i + 1) rest c

                _ ->
                    Nothing

        matchForward s t result =
            case String.uncons t of
                Just ( ch, rest ) ->
                    let
                        j =
                            result |> List.head |> Maybe.withDefault 0

                        --_ =
                        --    Debug.log "ch" ch
                        --_ =
                        --    Debug.log "rest" rest
                        --_ =
                        --    Debug.log "j" j
                    in
                        case (ch |> indexOf j (String.dropLeft j s)) of
                            Just i ->
                                matchForward s rest (i :: result)

                            _ ->
                                []

                _ ->
                    result

        matchBackward s t i j result =
            if i < 0 || j < 0 then
                result
            else if charAt i s == charAt j t then
                matchBackward s t (i - 1) (j - 1) (i :: result)
            else
                matchBackward s t (i - 1) j result

        indexes =
            matchForward s t []

        rindexes =
            case indexes of
                i :: _ ->
                    matchBackward s
                        t
                        (i - 1)
                        (String.length t - 2)
                        [ i ]

                _ ->
                    []
    in
        rindexes


fuzzyMatch : List String -> String -> List FuzzyMatchItem
fuzzyMatch src target =
    if target == "" then
        List.map
            (\s ->
                { text = s
                , matches = []
                }
            )
            src
    else
        src
            |> List.filterMap
                (\s ->
                    target
                        |> String.toLower
                        |> fuzzyMatchInner (String.toLower s)
                        |> (\matches ->
                                case matches of
                                    [] ->
                                        Nothing

                                    _ ->
                                        Just
                                            { text = s
                                            , matches = matches
                                            }
                           )
                )
            |> List.sortBy
                (\item ->
                    let
                        first =
                            item.matches
                                |> List.head
                                |> Maybe.withDefault 0

                        last =
                            item.matches
                                |> getLast
                                |> Maybe.withDefault 0
                    in
                        ( last - first, first )
                )
