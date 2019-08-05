module Vim.Helper exposing
    ( aggregateChanges
    , aggregateCount
    , aggregateKeys
    , aggregateModeName
    , aggregateOperator
    , aggregateRecordKeys
    , aggregateRecordingMacro
    , aggregateRegister
    , completeAndThen
    , countParser
    , dropLast
    , dropUntil
    , escapeKey
    , isBetween
    , isComplete
    , isEscaped
    , keyParser
    , makePushKeys
    , mapTuple
    , parseKeys
    , popComplete
    , popKey
    , pushComplete
    , readKeyAndThen
    , readKeysAndThen
    , tuple
    , visualAfterOperator
    , visualLineAfterOperator
    , visualRangeAfterOperator
    )

import Char
import Helper.Helper
    exposing
        ( getLast
        , notSpace
        , oneOrMore
        , repeatParser
        )
import List
import Parser as P exposing ((|.), (|=), Parser)
import Result
import String
import Vim.AST
    exposing
        ( Key
        , ModeDelta
        , ModeName(..)
        , Operator
        , OperatorRange(..)
        , Register
        , StateChange(..)
        , defaultRegister
        )


isBetween : Char -> Char -> Char -> Bool
isBetween low high char =
    let
        code =
            Char.toCode char
    in
    (code >= Char.toCode low) && (code <= Char.toCode high)


countParser : Parser Int
countParser =
    (P.getChompedString <|
        P.chompIf (isBetween '1' '9')
            |. P.chompWhile Char.isDigit
    )
        |> P.andThen
            (\s ->
                let
                    n =
                        String.toInt s
                in
                case n of
                    Just n_ ->
                        -- prevent too large count
                        if n_ > 2 ^ 30 then
                            P.problem "too large count"

                        else
                            P.succeed n_

                    _ ->
                        P.problem "not a valid count"
            )


dropLast : List a -> List a
dropLast l =
    case l of
        [] ->
            []

        [ x ] ->
            []

        x :: xs ->
            x :: dropLast xs


tuple : a -> b -> ( a, b )
tuple a b =
    ( a, b )


mapTuple : (a -> a1) -> (b -> b1) -> ( a, b ) -> ( a1, b1 )
mapTuple m1 m2 ( a, b ) =
    ( m1 a, m2 b )


readKeyAndThen :
    String
    -> ModeDelta
    -> Parser ModeDelta
    -> Parser ModeDelta
readKeyAndThen key partialResult nextOps =
    P.succeed identity
        |. P.symbol key
        |= P.oneOf
            [ P.succeed partialResult
                |. P.end
            , P.succeed []
                |. P.symbol "<escape>"
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
        |> P.getChompedString
    )
        |> P.andThen
            (\key ->
                P.oneOf
                    [ P.succeed
                        (partialResult key)
                        |. P.end
                    , P.succeed
                        []
                        |. P.symbol "<escape>"
                    , nextOps key
                    ]
            )


aggregateChanges : StateChange -> StateChange -> ModeDelta -> Bool
aggregateChanges push pop modeDelta =
    List.foldl
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
        > 0


isComplete : ModeDelta -> Bool
isComplete =
    aggregateChanges PushComplete PopComplete


isEscaped : ModeDelta -> Bool
isEscaped =
    aggregateChanges PushEscape PopEscape


aggregateCount : ModeDelta -> Maybe Int
aggregateCount =
    List.foldl
        (\change res ->
            case change of
                PushCount n ->
                    res
                        |> Maybe.withDefault 1
                        |> (*) n
                        |> Just

                PopCount ->
                    Maybe.map (always 1) res

                _ ->
                    res
        )
        Nothing


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
pushComplete delta =
    delta ++ [ PushComplete ]


popComplete : ModeDelta -> ModeDelta
popComplete delta =
    delta ++ [ PopComplete ]


makePushKeys : String -> String -> ModeDelta
makePushKeys k1 k2 =
    [ PushKey (k1 ++ k2) ]


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


dropUntil : a -> List a -> List a
dropUntil item items =
    case items of
        head :: tail ->
            if head == item then
                tail

            else
                dropUntil item tail

        _ ->
            []


aggregateRecordKeys : ModeDelta -> String
aggregateRecordKeys changes =
    case changes of
        change :: rest ->
            case change of
                PushKey key ->
                    key
                        ++ aggregateRecordKeys rest

                PauseRecording ->
                    dropUntil ContinueRecording rest
                        |> aggregateRecordKeys

                _ ->
                    aggregateRecordKeys rest

        _ ->
            ""


aggregateKeys : ModeDelta -> String
aggregateKeys changes =
    changes
        |> List.foldl
            (\change result ->
                case change of
                    PushKey _ ->
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

                    _ ->
                        keys
            )
            []
        |> String.join ""


visualAfterOperator : Key -> OperatorRange -> OperatorRange
visualAfterOperator key range =
    case key of
        "v" ->
            visualRangeAfterOperator range

        "V" ->
            visualLineAfterOperator range

        _ ->
            range


visualRangeAfterOperator : OperatorRange -> OperatorRange
visualRangeAfterOperator range =
    case range of
        MotionRange md mo ->
            let
                { inclusive, linewise } =
                    mo
            in
            MotionRange md
                { mo
                    | inclusive =
                        if linewise then
                            False

                        else
                            not inclusive
                    , linewise = False
                }

        _ ->
            range


visualLineAfterOperator : OperatorRange -> OperatorRange
visualLineAfterOperator range =
    case range of
        MotionRange md mo ->
            MotionRange md
                { mo | linewise = True }

        _ ->
            range


escapeKey : String -> Key
escapeKey key =
    if key == "<" || key == "\\" then
        "\\" ++ key

    else
        key


keyParser : Parser Key
keyParser =
    P.oneOf
        [ P.symbol "<"
            |. oneOrMore (\ch -> notSpace ch && ch /= '>')
            |. P.symbol ">"
            |> P.getChompedString
        , P.succeed identity
            |. P.symbol "\\"
            |= (P.oneOf
                    [ P.symbol "<"
                    , P.symbol "\\"
                    ]
                    |> P.getChompedString
               )
        , P.chompIf notSpace
            |> P.getChompedString
        ]


parseKeys : String -> List Key
parseKeys =
    P.run (repeatParser keyParser)
        >> Result.withDefault []
