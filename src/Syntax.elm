module Syntax exposing (..)

import Elm.Array as Array exposing (Array)
import Internal.TextBuffer as B exposing (Patch(..))
import Helper exposing (..)
import List


type alias Token =
    { classname : String
    , length : Int
    }


updateToken : Int -> (Token -> Token) -> List Token -> List Token
updateToken n f tokens =
    case tokens of
        token :: rest ->
            if n <= token.length then
                f token :: rest
            else
                token :: (updateToken n f rest)

        _ ->
            []


type alias Syntax =
    Array (List Token)


applyPatchToSyntax : Patch -> Syntax -> ( Syntax, Maybe Int )
applyPatchToSyntax patch syntax =
    let
        longerToken x len =
            updateToken
                x
                (\token -> { token | length = token.length + len })

        splitTokens x left tokens =
            case tokens of
                token :: rest ->
                    if x <= token.length then
                        ( (if x == 0 then
                            left
                           else
                            { token | length = x } :: left
                          )
                        , (if x == token.length then
                            rest
                           else
                            { token | length = token.length - x }
                                :: rest
                          )
                        )
                    else if List.isEmpty rest then
                        ( token :: left, [] )
                    else
                        splitTokens (x - token.length)
                            (token :: left)
                            rest

                _ ->
                    ( tokens, [] )

        splitLines lines =
            let
                max =
                    B.countLines lines - 1
            in
                ( B.getLine 0 lines |> Maybe.withDefault ""
                , B.sliceLines 1 max lines
                , lines
                    |> B.getLine max
                    |> Maybe.withDefault ""
                )

        insert ( y, x ) lines syntax =
            case Array.get y syntax of
                Just tokens ->
                    let
                        ( left, right ) =
                            splitTokens x [] tokens

                        top =
                            Array.slice 0 y syntax

                        bottom =
                            Array.slice (y + 1) (Array.length syntax) syntax

                        ( firstLine, middleLines, lastLine ) =
                            splitLines lines

                        classname =
                            (case List.head left of
                                Just x ->
                                    x.classname

                                _ ->
                                    List.head right
                                        |> Maybe.map .classname
                                        |> Maybe.withDefault ""
                            )

                        left1 =
                            case left of
                                x :: xs ->
                                    ({ x | length = x.length + String.length firstLine }
                                        :: xs
                                    )
                                        |> List.reverse

                                _ ->
                                    [ { length = String.length firstLine
                                      , classname = classname
                                      }
                                    ]
                    in
                        if B.isMutipleLine lines then
                            top
                                |> Array.push
                                    left1
                                |> flip Array.append
                                    (B.mapLines
                                        (\line ->
                                            [ { length = String.length line
                                              , classname = classname
                                              }
                                            ]
                                        )
                                        middleLines
                                    )
                                |> Array.push right
                                |> flip Array.append bottom
                        else
                            top
                                |> Array.push (left1 ++ right)
                                |> flip Array.append bottom

                _ ->
                    if y == 0 && x == 0 then
                        lines
                            |> B.mapLines
                                (\line ->
                                    [ { length = String.length line
                                      , classname = ""
                                      }
                                    ]
                                )
                    else
                        syntax
    in
        case patch of
            Insertion pos lines ->
                if B.isEmpty lines then
                    ( syntax, Nothing )
                else
                    ( insert pos lines syntax
                    , Just <| Tuple.first pos
                    )

            Deletion ( ya, xa ) ( yb, xb ) ->
                case Array.get ya syntax of
                    Just tokens ->
                        let
                            ( left, _ ) =
                                splitTokens xa [] tokens
                        in
                            case Array.get yb syntax of
                                Just tokens2 ->
                                    let
                                        ( _, right ) =
                                            splitTokens xb [] tokens2
                                    in
                                        ( syntax
                                            |> Array.slice 0 ya
                                            |> (if
                                                    List.isEmpty left
                                                        && List.isEmpty right
                                                then
                                                    identity
                                                else
                                                    Array.push
                                                        (List.reverse left ++ right)
                                               )
                                            |> flip Array.append
                                                (Array.slice
                                                    (yb + 1)
                                                    (Array.length syntax)
                                                    syntax
                                                )
                                        , Just ya
                                        )

                                _ ->
                                    ( syntax
                                        |> Array.slice 0 ya
                                        |> Array.push left
                                    , Just ya
                                    )

                    _ ->
                        ( syntax, Just ya )


applyPatchesToSyntax :
    List Patch
    -> Syntax
    -> ( Syntax, Maybe Int )
applyPatchesToSyntax patches syntax =
    List.foldl
        (\patch result ->
            let
                ( syntax, n ) =
                    result

                ( newSyntax, newn ) =
                    applyPatchToSyntax patch syntax
            in
                ( newSyntax, minMaybe n newn )
        )
        ( syntax, Nothing )
        patches
