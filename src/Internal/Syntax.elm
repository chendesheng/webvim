module Internal.Syntax exposing
    ( Syntax
    , Token
    , TokenType(..)
    , applyPatchToSyntax
    , getToken
    , iterateTokens
    , splitTokens
    , updateToken
    )

import Array as Array exposing (Array)
import Internal.Position exposing (Position)
import Internal.TextBuffer as B exposing (Patch(..))
import List


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
                token :: updateToken n f rest

        _ ->
            []


type alias Syntax =
    Array (List Token)


splitTokens : Int -> List Token -> ( List Token, List Token )
splitTokens x_ tokens_ =
    let
        splitTokensHelper x left tokens =
            case tokens of
                token :: rest ->
                    if x <= token.length then
                        ( if x == 0 then
                            left

                          else
                            { token | length = x } :: left
                        , if x == token.length then
                            rest

                          else
                            { token | length = token.length - x }
                                :: rest
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
    splitTokensHelper x_ [] tokens_


applyPatchToSyntax : Patch -> Syntax -> ( Syntax, Maybe Int )
applyPatchToSyntax patch syntax =
    let
        longerToken x len =
            updateToken
                x
                (\token -> { token | length = token.length + len })

        insert ( y, x ) lines syntax_ =
            case Array.get y syntax_ of
                Just tokens ->
                    let
                        ( left, right ) =
                            splitTokens x tokens

                        { tipe, classname } =
                            case List.head left of
                                Just x_ ->
                                    x_

                                _ ->
                                    right
                                        |> List.head
                                        |> Maybe.withDefault
                                            { tipe = TokenOther
                                            , classname = ""
                                            , length = 0
                                            }

                        expand tokens_ n =
                            case tokens_ of
                                first :: rest ->
                                    { first | length = first.length + n }
                                        :: rest

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
                                                ++ (if cnt == 1 then
                                                        right

                                                    else
                                                        []
                                                   )

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
                    bottom
                        |> Array.append middle
                        |> Array.append top

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
                                            (List.reverse left
                                                ++ right
                                            )
                                   )
                                |> (\toappend ->
                                        Array.append
                                            toappend
                                            (Array.slice
                                                (yb + 1)
                                                (Array.length syntax)
                                                syntax
                                            )
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


getToken : Position -> Syntax -> Maybe Token
getToken ( y, x ) syntax =
    case
        Array.get y syntax
    of
        Just tokens ->
            let
                getTokenHelper tokens_ x_ =
                    case tokens_ of
                        token :: rest ->
                            if x_ < token.length then
                                Just token

                            else
                                getTokenHelper rest (x_ - token.length)

                        _ ->
                            Nothing
            in
            getTokenHelper tokens x

        _ ->
            Nothing


iterateTokens :
    Bool
    -> (Position -> String -> Token -> a -> ( a, Bool ))
    -> B.TextBuffer
    -> Syntax
    -> Position
    -> Int
    -> a
    -> a
iterateTokens forward fn lines syntax ( y, x ) lineLimit init =
    let
        iterateTokensHelper :
            (Position -> String -> Token -> a -> ( a, Bool ))
            -> a
            -> Position
            -> String
            -> List Token
            -> ( a, Bool )
        iterateTokensHelper fn_ init_ ( y_, x1 ) line tokens =
            case tokens of
                token :: restTokens ->
                    let
                        x_ =
                            if x1 == -1 then
                                String.length line

                            else
                                x1

                        res =
                            fn_
                                ( y_, x_ )
                                line
                                token
                                init_

                        ( next, stop ) =
                            res
                    in
                    if stop then
                        res

                    else
                        iterateTokensHelper
                            fn_
                            next
                            ( y_
                            , if forward then
                                x_ + token.length

                              else
                                x_ - token.length
                            )
                            line
                            restTokens

                _ ->
                    ( init_, False )

        result =
            Maybe.map2
                (iterateTokensHelper fn init ( y, x ))
                (B.getLine y lines)
                (syntax
                    |> Array.get y
                    |> Maybe.map
                        (if x == -1 then
                            List.reverse

                         else
                            splitTokens x
                                >> (if forward then
                                        Tuple.second

                                    else
                                        Tuple.first
                                   )
                        )
                )
    in
    case result of
        Just ( next, stop ) ->
            if stop then
                next

            else if forward && y >= lineLimit then
                init

            else if not forward && y < lineLimit then
                init

            else
                iterateTokens forward
                    fn
                    lines
                    syntax
                    (if forward then
                        ( y + 1, 0 )

                     else
                        ( y - 1, -1 )
                    )
                    lineLimit
                    next

        _ ->
            init
