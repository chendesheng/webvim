module Vim.Helper exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Char
import String
import List
import Result
import Vim.AST
    exposing
        ( ModeDelta
        , StateChange(..)
        , Operator
        , ModeName(..)
        , Key
        , Register
        , defaultRegister
        , OperatorRange(..)
        , Inclusive(..)
        )


escapeKey : String -> Key
escapeKey key =
    if key == "<" then
        "\\<"
    else if key == "\\" then
        "\\\\"
    else
        key


isBetween : Char -> Char -> Char -> Bool
isBetween low high char =
    let
        code =
            Char.toCode char
    in
        (code >= Char.toCode low) && (code <= Char.toCode high)


countParser : Parser Int
countParser =
    (P.source <|
        P.ignore (P.Exactly 1) (isBetween '1' '9')
            |. P.ignore P.zeroOrMore Char.isDigit
    )
        |> P.andThen
            (\s ->
                let
                    n =
                        String.toInt s
                in
                    case n of
                        Ok n ->
                            -- prevent too large count
                            if n > 2 ^ 30 then
                                P.fail "too large count"
                            else
                                P.succeed n

                        _ ->
                            P.fail "not a valid count"
            )


ignoreChar : (Char -> Bool) -> Parser ()
ignoreChar pred =
    P.ignore (P.Exactly 1) pred


keepChar : (Char -> Bool) -> Parser String
keepChar pred =
    P.keep (P.Exactly 1) pred


dropLast : List a -> List a
dropLast l =
    case l of
        [] ->
            []

        [ x ] ->
            []

        x :: xs ->
            x :: dropLast xs


getLast : List a -> Maybe a
getLast xs =
    case xs of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            getLast xs


tuple : a -> b -> ( a, b )
tuple a b =
    ( a, b )


tupleFlip : b -> a -> ( a, b )
tupleFlip =
    flip tuple


mapTuple : (a -> a1) -> (b -> b1) -> ( a, b ) -> ( a1, b1 )
mapTuple m1 m2 ( a, b ) =
    ( m1 a, m2 b )


readKeyAndThen :
    String
    -> ModeDelta
    -> Parser ModeDelta
    -> Parser ModeDelta
readKeyAndThen key partialResult nextOps =
    P.succeed
        identity
        |. P.symbol key
        |= P.oneOf
            [ P.succeed
                partialResult
                |. P.end
            , P.succeed
                []
                |. P.symbol "<esc>"
            , nextOps
            ]


readKeysAndThen :
    List Key
    -> (Key -> ModeDelta)
    -> (Key -> Parser ModeDelta)
    -> Parser ModeDelta
readKeysAndThen keys partialResult nextOps =
    (P.oneOf
        (List.map P.symbol keys)
        |> P.source
    )
        |> P.andThen
            (\key ->
                P.oneOf
                    [ P.succeed
                        (partialResult key)
                        |. P.end
                    , P.succeed
                        []
                        |. P.symbol "<esc>"
                    , (nextOps key)
                    ]
            )


aggregateChanges : StateChange -> StateChange -> ModeDelta -> Bool
aggregateChanges push pop modeDelta =
    (List.foldl
        (\change res ->
            if change == push then
                res + 1
            else if change == pop then
                res - 1
            else
                res
        )
        0
        modeDelta
    )
        > 0


isComplete : ModeDelta -> Bool
isComplete =
    aggregateChanges PushComplete PopComplete


aggregateCount : ModeDelta -> Int
aggregateCount =
    List.foldl
        (\change res ->
            case change of
                PushCount n ->
                    res * n

                _ ->
                    res
        )
        1


aggregateRegister : ModeDelta -> Register
aggregateRegister modeDelta =
    modeDelta
        |> List.filter
            (\change ->
                case change of
                    PushRegister reg ->
                        True

                    _ ->
                        False
            )
        |> getLast
        |> Maybe.map
            (\change ->
                case change of
                    PushRegister reg ->
                        reg

                    _ ->
                        defaultRegister
            )
        |> Maybe.withDefault defaultRegister


aggregateRecordingMacro : ModeDelta -> Maybe Register
aggregateRecordingMacro modeDelta =
    List.foldl
        (\change ops ->
            case change of
                PushRecordMacro op ->
                    op :: ops

                PopRecordMacro ->
                    List.tail ops
                        |> Maybe.withDefault []

                _ ->
                    ops
        )
        []
        modeDelta
        |> List.head


completeAndThen :
    (ModeDelta -> Parser ModeDelta)
    -> Parser ModeDelta
    -> Parser ModeDelta
completeAndThen f p =
    P.andThen
        (\modeDelta ->
            if isComplete modeDelta then
                f modeDelta
            else
                P.succeed modeDelta
        )
        p


popKey : ModeDelta -> Parser ModeDelta
popKey modeDelta =
    (modeDelta ++ [ PopKey ])
        |> P.succeed


pushComplete : ModeDelta -> ModeDelta
pushComplete =
    flip (++) [ PushComplete ]


popComplete : ModeDelta -> ModeDelta
popComplete =
    flip (++) [ PopComplete ]


makePushKeys : String -> String -> ModeDelta
makePushKeys k1 k2 =
    [ PushKeys [ k1, k2 ] ]


aggregateOperator : ModeDelta -> Maybe Operator
aggregateOperator modeDelta =
    List.foldl
        (\change ops ->
            case change of
                PushOperator op ->
                    op :: ops

                PopOperator ->
                    List.tail ops
                        |> Maybe.withDefault []

                _ ->
                    ops
        )
        []
        modeDelta
        |> List.head


aggregateModeName : ModeDelta -> ModeName
aggregateModeName changes =
    List.foldl
        (\change modes ->
            case change of
                PushMode m ->
                    m :: modes

                PopMode ->
                    List.tail modes
                        |> Maybe.withDefault []

                _ ->
                    modes
        )
        []
        changes
        |> List.head
        |> Maybe.withDefault ModeNameNormal


aggregateKeys : ModeDelta -> String
aggregateKeys changes =
    changes
        |> List.foldl
            (\change result ->
                case change of
                    PushKey _ ->
                        change :: result

                    PushKeys _ ->
                        change :: result

                    PopKey ->
                        List.tail result
                            |> Maybe.withDefault []

                    _ ->
                        result
            )
            []
        |> List.foldl
            (\change keys ->
                case change of
                    PushKey key ->
                        key :: keys

                    PushKeys keys2 ->
                        keys2 ++ keys

                    _ ->
                        keys
            )
            []
        |> List.map escapeKey
        |> String.join ""


flipInclusive : OperatorRange -> OperatorRange
flipInclusive range =
    case range of
        MotionRange Inclusive m ->
            MotionRange Exclusive m

        MotionRange Exclusive m ->
            MotionRange Inclusive m

        _ ->
            range


escapedChar : Parser String
escapedChar =
    P.oneOf
        -- escape '<' and '\'
        [ P.succeed identity
            |. ignoreChar ((==) '\\')
            |= keepChar (\ch -> ch == '<' || ch == '\\')
        , keepChar (\ch -> ch /= '<')
        ]


keyParser : Parser Key
keyParser =
    P.oneOf
        [ escapedChar
        , (P.symbol "<" |. P.ignoreUntil ">")
            |> P.source
        ]
