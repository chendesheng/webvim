module Vim.Parser exposing (..)

import Vim.Key exposing (..)
import Vim.State exposing (..)
import Vim.Helper exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Vim.Register exposing (..)


insertMode : (InsertCommand -> Operator) -> Parser ModeDelta
insertMode map =
    P.succeed ((++) [ PushMode ModeNameInsert ])
        |= P.oneOf
            [ readKeyAndThen "<c-r>"
                [ PushKey "<c-r>" ]
                []
                (P.oneOf
                    [ P.succeed
                        (\key ->
                            if isRegister key then
                                [ PushRegister key
                                , PutRegister
                                    |> map
                                    |> PushOperator
                                ]
                            else
                                []
                        )
                        |= keyParser
                    ]
                )
            , readKeyAndThen "<c-o>"
                [ PushKey "<c-o>", PushMode ModeNameTempNormal ]
                []
                (P.andThen
                    (\modeDelta ->
                        let
                            modeName =
                                aggregateModeName modeDelta

                            op =
                                aggregateOperator modeDelta
                        in
                            P.succeed
                                (case op of
                                    Nothing ->
                                        if
                                            isComplete modeDelta
                                                || modeName
                                                == ModeNameInsert
                                        then
                                            []
                                        else
                                            [ PushMode ModeNameTempNormal
                                            , PushKey "<c-o>"
                                            ]
                                                ++ modeDelta

                                    Just op1 ->
                                        [ PushOperator op1 ]
                                )
                    )
                    (P.lazy (\_ -> operator False))
                )
            , P.oneOf
                [ P.symbol "<c-h>"
                , P.symbol "<backspace>"
                , P.symbol "<delete>"
                ]
                |> P.map
                    (\key ->
                        [ DeleteChar Backward |> map |> PushOperator ]
                    )
            , P.succeed
                [ DeleteWord Backward |> map |> PushOperator
                ]
                |. P.symbol "<c-w>"
            , P.succeed
                (\ch ->
                    [ InsertString ch |> map |> PushOperator ]
                )
                |= keyParser
            ]


linebuffer :
    String
    -> (ExCommand -> Operator)
    -> Parser ModeDelta
linebuffer prefix map =
    let
        define key op =
            (P.succeed
                [ PushOperator <| map op, PushComplete ]
            )
                |. P.symbol key
    in
        P.succeed ((++) [ PushKey prefix, PushMode <| ModeNameEx prefix ])
            |= P.oneOf
                [ P.succeed
                    [ PushOperator <| map Execute
                    , PopMode
                    , PushComplete
                    ]
                    |. P.symbol "<cr>"
                , P.succeed
                    [ PopMode, PushComplete ]
                    |. P.symbol "<esc>"
                , define "<c-h>" (DeleteChar Backward |> ExInsert)
                , define "<backspace>" (DeleteChar Backward |> ExInsert)
                , define "<delete>" (DeleteChar Backward |> ExInsert)
                , define "<c-w>" (DeleteWord Backward |> ExInsert)
                , define "<c-e>" (ExInsert GotoLineEnd)
                , define "<c-b>" (ExInsert GotoLineStart)
                , readKeyAndThen "<c-r>"
                    [ PushKey "<c-r>" ]
                    []
                    (P.oneOf
                        [ P.succeed
                            [ InsertWordUnderCursor |> map |> PushOperator ]
                            |. P.symbol "<c-w>"
                        , P.succeed
                            (\key ->
                                if isRegister key then
                                    [ PushRegister key
                                    , PutRegister
                                        |> ExInsert
                                        |> map
                                        |> PushOperator
                                    ]
                                else
                                    []
                            )
                            |= keyParser
                        ]
                    )
                , P.succeed
                    (\ch ->
                        [ InsertString ch
                            |> ExInsert
                            |> map
                            |> PushOperator
                        ]
                    )
                    |= keyParser
                ]


textObject :
    (TextObject -> Bool -> Operator)
    -> Parser ModeDelta
textObject map =
    let
        define pair obj =
            P.succeed identity
                |= P.oneOf
                    [ readKeyAndThen "i"
                        [ PushKey "i" ]
                        []
                        (P.succeed
                            [ map obj False |> PushOperator
                            , PushKeys [ "i", pair ]
                            , PushComplete
                            ]
                            |. P.symbol pair
                        )
                    , readKeyAndThen "a"
                        [ PushKey "a" ]
                        []
                        (P.succeed
                            [ map obj True |> PushOperator
                            , PushKeys [ "a", pair ]
                            , PushComplete
                            ]
                            |. P.symbol pair
                        )
                    ]
    in
        P.oneOf
            [ define "w" Word
            , define "W" WORD
            , define "(" (Pair '(')
            , define ")" (Pair ')')
            , define "{" (Pair '{')
            , define "}" (Pair '}')
            , define "[" (Pair '[')
            , define "]" (Pair ']')
            , define "b" (Pair '(')
            , define "B" (Pair '{')
            , define "\\<" (Pair '<')
            , define ">" (Pair '>')
            , define "t" (Pair 't')
            , define "w" Word
            , define "W" WORD
            , define "(" (Pair '(')
            , define ")" (Pair ')')
            , define "{" (Pair '{')
            , define "}" (Pair '}')
            , define "[" (Pair '[')
            , define "]" (Pair ']')
            , define "b" (Pair '(')
            , define "B" (Pair '{')
            , define "\\<" (Pair '<')
            , define ">" (Pair '>')
            , define "t" (Pair 't')
            ]


gMotion :
    (Motion -> Operator)
    -> Parser ModeDelta
gMotion map =
    readKeyAndThen "g"
        [ PushKey "g" ]
        [ PushKeys [ "g", "<esc>" ], PushComplete ]
        (P.oneOf
            [ gMotionPart map
            , P.succeed (makePushKeys "g" >> pushComplete)
                |= keyParser
            ]
        )


gOperator : Parser ModeDelta
gOperator =
    let
        define key modeDelta =
            P.succeed (pushComplete modeDelta) |. P.symbol key
    in
        readKeyAndThen "g"
            [ PushKey "g" ]
            []
            (P.oneOf
                [ gMotionPart Move
                , define "J"
                    [ PushOperator <| Join True
                    , PushKeys [ "g", "J" ]
                    , PushComplete
                    ]
                , P.succeed (makePushKeys "g" >> pushComplete)
                    |= keyParser
                ]
            )


gMotionPart : (Motion -> Operator) -> Parser ModeDelta
gMotionPart map =
    let
        define key op =
            P.succeed
                [ PushOperator <| map op
                , PushKeys [ "g", key ]
                , PushComplete
                ]
                |. P.symbol key
    in
        P.oneOf
            [ define "g" (LineNumber 0)
            , define "j" (VLineDelta 1)
            , define "k" (VLineDelta -1)
            , define "n" (MatchString Forward)
            , define "N" (MatchString Backward)
            ]


motion :
    (Motion -> Operator)
    ->
        ((Motion -> Operator)
         -> Parser ModeDelta
        )
    -> Parser ModeDelta
motion map gMotion =
    let
        byClass ch direction class =
            P.succeed
                [ ByClass { direction = direction, class = class }
                    |> map
                    |> PushOperator
                , PushKey ch
                , PushComplete
                ]
                |. P.symbol ch

        matchChar trigger direction inclusive =
            readKeyAndThen trigger
                [ PushKey trigger ]
                [ PushKeys [ trigger, "<esc>" ]
                , PushComplete
                ]
                (P.succeed
                    (\ch ->
                        [ MatchChar
                            { char = ch
                            , direction = direction
                            , inclusive = inclusive
                            }
                            |> map
                            |> PushOperator
                        , PushKeys [ trigger, ch ]
                        , PushComplete
                        ]
                    )
                    |= keyParser
                )

        define ch m =
            P.succeed [ map m |> PushOperator, PushKey ch, PushComplete ]
                |. P.symbol ch

        matchString prefix direction =
            readKeyAndThen prefix
                [ PushKey prefix, PushMode <| ModeNameEx prefix ]
                []
                (linebuffer prefix
                    (\cmd ->
                        if cmd == Execute then
                            MatchString direction
                                |> map
                        else
                            ExMode cmd
                    )
                )
    in
        P.oneOf
            [ byClass "b" Backward WordStart
            , byClass "B" Backward WORDStart
            , byClass "e" Forward WordEnd
            , byClass "E" Forward WORDEnd
            , byClass "w" Forward WordStart
            , byClass "W" Forward WORDEnd
            , byClass "h" Backward CharStart
            , define "j" <| LineDelta 1
            , define "k" <| LineDelta -1
            , byClass "l" Forward CharStart
            , byClass "^" Backward LineFirst
            , byClass "0" Backward LineStart
            , byClass "$" Forward LineEnd
            , define "G" LastLine
            , matchChar "f" Forward Inclusive
            , matchChar "F" Backward Inclusive
            , matchChar "t" Forward Exclusive
            , matchChar "T" Backward Exclusive
            , define "H" ViewTop
            , define "M" ViewMiddle
            , define "L" ViewBottom
            , define "%" MatchPair
            , gMotion map
            , matchString "/" Forward
            , matchString "?" Backward
            ]


operator : Bool -> Parser ModeDelta
operator isVisual =
    let
        define key op =
            (P.succeed
                [ PushOperator op
                , PushKey key
                , PushComplete
                ]
                |. P.symbol key
            )

        startInsert key op =
            readKeyAndThen key
                (op ++ [ PushMode ModeNameInsert, PushKey key ])
                [ PopMode, PushComplete ]
                (P.map ((++) [ PushKey key ])
                    (insertMode InsertMode)
                )

        rangeOperator key op opop =
            P.oneOf
                [ P.succeed
                    [ PushKey key
                    , PushOperator opop
                    , PushComplete
                    ]
                    |. P.symbol key
                , keyParserAndThen
                    (P.oneOf
                        [ P.symbol "v"
                        , P.symbol "V"
                        , P.symbol "<c-v>"
                        ]
                        |> P.source
                    )
                    (\key1 -> [ PushKeys [ key, key1 ] ])
                    []
                    (P.map
                        ((++) [ PushKeys [ key, "v" ] ])
                        (P.oneOf
                            [ (motion
                                (MotionRange Inclusive >> op)
                                gMotion
                              )
                            , (textObject
                                (\obj around ->
                                    TextObject obj around |> op
                                )
                              )
                            ]
                        )
                    )
                , P.map ((++) [ PushKey key ])
                    (P.oneOf
                        [ motion
                            (MotionRange Exclusive >> op)
                            gMotion
                        , textObject
                            (\obj around ->
                                TextObject obj around |> op
                            )
                        ]
                    )
                ]
                |> readKeyAndThen key
                    [ PushKey key ]
                    []
                |> completeAndThen
                    (\modeDelta ->
                        if key == "c" then
                            (P.map ((++) (popComplete modeDelta))
                                (P.oneOf
                                    [ P.succeed
                                        [ PushMode ModeNameInsert ]
                                        |. P.end
                                    , P.succeed
                                        [ PopMode
                                        , PopOperator
                                        , PushComplete
                                        ]
                                        |. P.symbol "<esc>"
                                    , (P.map ((++) [ PopOperator ])
                                        (insertMode InsertMode)
                                      )
                                    ]
                                )
                            )
                                |> completeAndThen parserPopKey
                        else
                            parserPopKey modeDelta
                    )
    in
        P.oneOf
            [ (if isVisual then
                textObject (\obj around -> Select obj around)
               else
                P.fail "skip"
              )
            , motion Move (\_ -> P.oneOf [])
            , registerParser
                |> P.andThen
                    (\modeDelta ->
                        (P.oneOf
                            [ P.map ((++) modeDelta)
                                (completeAndThen parserPopKey <|
                                    operator isVisual
                                )
                            , P.succeed modeDelta
                            ]
                        )
                    )
            , countParser
                |> P.andThen
                    (\cnt ->
                        let
                            modeDelta =
                                [ PushCount cnt
                                , PushKey (toString cnt)
                                ]
                        in
                            (P.oneOf
                                [ P.map ((++) modeDelta) <| operator isVisual
                                , P.map (always modeDelta) P.end
                                , P.succeed []
                                ]
                            )
                    )
            , startInsert "i" []
            , startInsert "I"
                [ ByClass
                    { class = LineStart
                    , direction = Backward
                    }
                    |> Move
                    |> PushOperator
                ]
            , startInsert "a"
                [ ByClass
                    { class = CharStart
                    , direction = Forward
                    }
                    |> Move
                    |> PushOperator
                ]
            , startInsert "A"
                [ ByClass
                    { class = LineEnd
                    , direction = Forward
                    }
                    |> Move
                    |> PushOperator
                ]
            , startInsert "D"
                [ (MotionRange Exclusive
                    (ByClass
                        { class = LineEnd
                        , direction = Forward
                        }
                    )
                  )
                    |> Delete
                    |> PushOperator
                ]
            , startInsert "s"
                [ (MotionRange Exclusive
                    (ByClass { class = CharStart, direction = Forward })
                  )
                    |> Delete
                    |> PushOperator
                ]
            , startInsert "S"
                [ TextObject Line False |> Delete |> PushOperator ]
            , startInsert "o" [ OpenNewLine Forward |> PushOperator ]
            , startInsert "O" [ OpenNewLine Backward |> PushOperator ]
            , rangeOperator "d"
                Delete
                (Delete <| TextObject Line True)
            , rangeOperator "c"
                Delete
                (Delete <| TextObject Line False)
            , rangeOperator "y"
                Yank
                (Yank <| TextObject Line True)
            , rangeOperator ">"
                (Indent Forward)
                (Indent Forward <| TextObject Line False)
            , rangeOperator "\\<"
                (Indent Backward)
                (Indent Backward <| TextObject Line False)
            , readKeyAndThen ":"
                [ PushKey ":", PushMode <| ModeNameEx ":" ]
                []
                (linebuffer ":" ExMode)
            , readKeyAndThen "@"
                [ PushKey "@" ]
                []
                (P.map
                    (\key ->
                        if isRegister key then
                            [ PushKeys [ "@", key ]
                            , ReplayMacro key |> PushOperator
                            , PushComplete
                            ]
                        else
                            []
                    )
                    keyParser
                )
            , define "u" Undo
            , define "<c-r>" Redo
            , define "<c-o>" (JumpHistory Backward)
            , define "<tab>" (JumpHistory Forward)
            , define "<c-u>" (JumpByView 0.5)
            , define "<c-d>" (JumpByView -0.5)
            , define "<c-f>" (JumpByView 1)
            , define "<c-b>" (JumpByView -1)
            , define "<c-y>" (Scroll 1)
            , define "<c-e>" (Scroll -1)
            , define "<c-p>" (CompleteWord Backward)
            , define "<c-n>" (CompleteWord Forward)
            , define "J" (Join False)
            , define "x"
                (ByClass { class = CharStart, direction = Forward }
                    |> MotionRange Exclusive
                    |> Delete
                )
            , define "X"
                (ByClass { class = CharStart, direction = Backward }
                    |> MotionRange Exclusive
                    |> Delete
                )
            , gOperator
            , P.lazy (\_ -> macro isVisual)
            , P.lazy (\_ -> visual)
            ]


visual : Parser ModeDelta
visual =
    let
        key2mode key =
            case key of
                "v" ->
                    ModeNameVisual

                "V" ->
                    ModeNameVisualLine

                "<c-v>" ->
                    ModeNameVisualBlock

                _ ->
                    ModeNameVisual
    in
        P.oneOf
            [ P.symbol "v"
            , P.symbol "V"
            , P.symbol "<c-v>"
            ]
            |> P.source
            |> P.andThen
                (\key ->
                    P.oneOf
                        [ P.succeed
                            (\key1 ->
                                if key1 == key || key1 == "<esc>" then
                                    []
                                else if key1 == "" then
                                    [ key2mode key |> PushMode, PushKey key ]
                                else
                                    [ key2mode key1 |> PushMode, PushKey key1 ]
                            )
                            |= (P.oneOf
                                    [ P.symbol "v"
                                    , P.symbol "V"
                                    , P.symbol "<c-v>"
                                    , P.symbol "<esc>"
                                    , P.end
                                    ]
                                    |> P.source
                               )
                        , P.succeed
                            ((++)
                                [ key2mode key |> PushMode
                                , PushKey key
                                ]
                            )
                            |= P.oneOf
                                [ textObject
                                    (\obj around -> Select obj around)
                                , operator True
                                ]
                        ]
                )


macro : Bool -> Parser ModeDelta
macro isVisual =
    readKeyAndThen "q"
        [ PushKey "q" ]
        []
        (keyParser
            |> P.andThen
                (\key ->
                    if isRegister key then
                        P.succeed
                            [ PushKeys [ "q", key ]
                            , PushRecordMacro key
                            ]
                    else
                        P.fail ("unknown register: " ++ key)
                )
            |> P.andThen
                (\modeDelta ->
                    P.map ((++) modeDelta)
                        (P.oneOf
                            [ P.succeed [ PopRecordMacro, PushComplete ]
                                |. P.symbol "q"
                            , operator isVisual
                            , P.succeed []
                            ]
                        )
                )
        )


parse : String -> Key -> State
parse lastKeys key =
    if key == "<c-c>" then
        ( initialMode, "" )
    else
        let
            keys =
                lastKeys ++ escapeKey key

            modeDelta =
                P.run (completeAndThen parserPopKey <| operator False) keys
                    |> Result.withDefault []
        in
            ( { count = aggregateCount modeDelta
              , edit = aggregateOperator modeDelta
              , register = aggregateRegister modeDelta
              , mode = aggregateModeName modeDelta
              , recordMacro = aggregateRecordingMacro modeDelta
              }
            , aggregateKeys modeDelta
            )
