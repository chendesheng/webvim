module Syntax exposing (..)

import Elm.Array as Array exposing (Array)
import Internal.TextBuffer as B exposing (Patch(..))
import Helper exposing (..)
import List
import Position exposing (Position)


type TokenType
    = TokenComment
    | TokenString
    | TokenRegex
    | TokenOther


type alias Token =
    { tipe : TokenType
    , classname : String
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


splitTokens : Int -> List Token -> ( List Token, List Token )
splitTokens x tokens =
    let
        splitTokensHelper x left tokens =
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
                        splitTokensHelper (x - token.length)
                            (token :: left)
                            rest

                _ ->
                    ( [], [] )
    in
        splitTokensHelper x [] tokens


applyPatchToSyntax : Patch -> Syntax -> ( Syntax, Maybe Int )
applyPatchToSyntax patch syntax =
    let
        longerToken x len =
            updateToken
                x
                (\token -> { token | length = token.length + len })

        insert ( y, x ) lines syntax =
            case Array.get y syntax of
                Just tokens ->
                    let
                        ( left, right ) =
                            splitTokens x tokens

                        { tipe, classname } =
                            (case List.head left of
                                Just x ->
                                    x

                                _ ->
                                    right
                                        |> List.head
                                        |> Maybe.withDefault
                                            { tipe = TokenOther
                                            , classname = ""
                                            , length = 0
                                            }
                            )

                        expand tokens n =
                            case tokens of
                                x :: xs ->
                                    ({ x | length = x.length + n }
                                        :: xs
                                    )

                                _ ->
                                    [ { length = n
                                      , classname = classname
                                      , tipe = tipe
                                      }
                                    ]

                        cnt =
                            B.count lines

                        middle =
                            lines
                                |> B.mapLines String.length
                                |> Array.indexedMap
                                    (\i n ->
                                        if i == 0 then
                                            (expand left n
                                                |> List.reverse
                                            )
                                                ++ if cnt == 1 then
                                                    right
                                                   else
                                                    []
                                        else if i == cnt - 1 then
                                            expand right n
                                        else
                                            [ { length = n
                                              , classname = classname
                                              , tipe = tipe
                                              }
                                            ]
                                    )

                        top =
                            Array.slice 0 y syntax

                        bottom =
                            Array.slice (y + 1) (Array.length syntax) syntax
                    in
                        top
                            |> flip Array.append middle
                            |> flip Array.append bottom

                _ ->
                    lines
                        |> B.mapLines
                            (\line ->
                                [ { length = String.length line
                                  , classname = ""
                                  , tipe = TokenOther
                                  }
                                ]
                            )
                        |> Array.append syntax
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
                                splitTokens xa tokens
                        in
                            case Array.get yb syntax of
                                Just tokens2 ->
                                    let
                                        ( _, right ) =
                                            splitTokens xb tokens2
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


getToken : Position -> Syntax -> Maybe Token
getToken ( y, x ) syntax =
    case
        Array.get y syntax
    of
        Just tokens ->
            let
                getTokenHelper tokens x =
                    case tokens of
                        token :: rest ->
                            if x < token.length then
                                Just token
                            else
                                getTokenHelper rest (x - token.length)

                        _ ->
                            Nothing
            in
                getTokenHelper tokens x

        _ ->
            Nothing
