module Helper.Fuzzy exposing (FuzzyMatchItem, fuzzyMatch, fuzzyMatchInner, normalizeSlash)

import Char
import Helper.Helper exposing (getLast)
import List
import String


type alias FuzzyMatchItem =
    { text : String
    , matches : List Int
    }


normalizeSlash : String -> String
normalizeSlash a =
    if a == "/" || a == "\\" then
        "/"

    else
        a


fuzzyMatchInner : String -> String -> List Int
fuzzyMatchInner s t =
    let
        smartCaseEqual a b =
            if String.any Char.isLower b then
                String.toLower a == b

            else
                a == b

        equal a b =
            smartCaseEqual (normalizeSlash a) (normalizeSlash b)

        lenS =
            String.length s

        lenT =
            String.length t

        charAt i =
            String.slice i (i + 1)

        match delta i j result =
            if i < 0 || j < 0 || i >= lenS || j >= lenT then
                result

            else if equal (charAt i s) (charAt j t) then
                match delta (i + delta) (j + delta) (i :: result)

            else
                match delta (i + delta) j result

        matchForward i j result =
            match 1 i j result

        matchBackward i j result =
            match -1 i j result

        indexes =
            matchForward 0 0 []
    in
    if List.length indexes == lenT then
        case indexes of
            i :: _ ->
                matchBackward
                    (i - 1)
                    (lenT - 2)
                    [ i ]

            _ ->
                []

    else
        []


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
                        |> fuzzyMatchInner s
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
