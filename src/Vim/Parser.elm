module Vim.Parser exposing (..)

import Vim.AST exposing (..)
import Vim.Helper exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Vim.Register exposing (..)


insertCommands : Parser ModeDelta
insertCommands =
    --common insert commands
    let
        define key op =
            (P.succeed
                [ PushOperator <| op ]
            )
                |. P.symbol key

        deleteCharBackward =
            { class = CharStart, direction = Backward }
                |> ByClass
                |> MotionRange Inclusive
                |> Delete

        deleteWordBackward =
            { class = WordStart, direction = Backward }
                |> ByClass
                |> MotionRange Inclusive
                |> Delete

        gotoLineEnd =
            { class = LineEnd, direction = Forward }
                |> ByClass
                |> Move

        gotoLineStart =
            { class = LineStart, direction = Backward }
                |> ByClass
                |> Move
    in
        P.oneOf
            [ define "<c-h>" deleteCharBackward
            , define "<backspace>" deleteCharBackward
            , define "<delete>" deleteCharBackward
            , define "<c-w>" deleteWordBackward
            , define "<c-e>" gotoLineEnd
            , define "<c-b>" gotoLineStart
            , P.succeed
                [ PopMode
                , PushComplete
                ]
                |. P.symbol "<esc>"
            , P.succeed
                (\ch ->
                    let
                        s =
                            case ch of
                                "<cr>" ->
                                    "\n"

                                "<tab>" ->
                                    "\t"

                                _ ->
                                    ch
                    in
                        [ InsertString s
                            |> PushOperator
                        ]
                )
                |= keyParser
            ]


insertMode : Parser ModeDelta
insertMode =
    let
        tempNormalMode =
            readKeyAndThen "<c-o>"
                [ PushKey "<c-o>", PushMode ModeNameTempNormal ]
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

        register =
            readKeyAndThen "<c-r>"
                [ PushKey "<c-r>" ]
                (P.succeed
                    (\key ->
                        if isRegister key then
                            [ PushRegister key
                            , Put Forward |> PushOperator
                            ]
                        else
                            []
                    )
                    |= keyParser
                )
    in
        P.succeed ((::) (PushMode ModeNameInsert))
            |= P.oneOf
                [ register
                , tempNormalMode
                , insertCommands
                ]


linebuffer : String -> (Operator -> Operator) -> Parser ModeDelta
linebuffer prefix map =
    let
        define key op =
            (P.succeed
                [ PushOperator <| op, PushComplete ]
            )
                |. P.symbol key
    in
        P.succeed ((++) [ PushKey prefix, PushMode <| ModeNameEx prefix ])
            |= P.oneOf
                [ P.succeed
                    [ ExecuteLine |> map |> PushOperator
                    , PopMode
                    , PushComplete
                    ]
                    |. P.symbol "<cr>"
                , readKeyAndThen "<c-r>"
                    [ PushKey "<c-r>" ]
                    (P.oneOf
                        [ P.succeed
                            [ PushOperator InsertWordUnderCursor ]
                            |. P.symbol "<c-w>"
                        , P.succeed
                            (\key ->
                                if isRegister key then
                                    [ PushRegister key
                                    , Put Forward
                                        |> PushOperator
                                    ]
                                else
                                    []
                            )
                            |= keyParser
                        ]
                    )
                , insertCommands
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
                        (P.succeed
                            [ map obj False |> PushOperator
                            , PushKeys [ "i", pair ]
                            , PushComplete
                            ]
                            |. P.symbol pair
                        )
                    , readKeyAndThen "a"
                        [ PushKey "a" ]
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


gKey :
    (Motion -> Operator)
    -> Parser ModeDelta
    -> Parser ModeDelta
gKey map extra =
    let
        define key op =
            P.succeed
                [ PushOperator <| map op
                , PushKeys [ "g", key ]
                , PushComplete
                ]
                |. P.symbol key
    in
        readKeyAndThen "g"
            [ PushKey "g" ]
            (P.oneOf
                [ P.oneOf
                    [ define "g" (LineNumber 0)
                    , define "j" (VLineDelta 1)
                    , define "k" (VLineDelta -1)
                    , define "n" (MatchString Forward)
                    , define "N" (MatchString Backward)
                    ]
                , extra
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
        gKey Move <|
            P.oneOf
                [ define "J"
                    [ PushOperator <| Join True
                    , PushKeys [ "g", "J" ]
                    , PushComplete
                    ]
                ]


motion :
    (Motion -> Operator)
    -> Parser ModeDelta
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
                (linebuffer prefix
                    (\cmd ->
                        if cmd == ExecuteLine then
                            MatchString direction
                                |> map
                        else
                            cmd
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
            , matchString "/" Forward
            , matchString "?" Backward
            , gMotion
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

        startInsert =
            P.oneOf
                [ P.succeed
                    [ PushMode ModeNameInsert ]
                    |. P.end
                , (P.map ((::) PopOperator) insertMode)
                ]

        defineInsert key op =
            (P.succeed identity
                |. P.symbol key
                |= startInsert
            )
                |> P.map ((++) (op ++ [ PushKey key ]))
                |> completeAndThen popKey

        operatorRange : (OperatorRange -> Operator) -> Parser ModeDelta
        operatorRange map =
            P.oneOf
                [ (motion
                    (MotionRange Exclusive >> map)
                    (gKey (MotionRange Exclusive >> map) <| P.oneOf [])
                  )
                , (textObject
                    (\obj around ->
                        TextObject obj around |> map
                    )
                  )
                ]

        operatorVisualRange key map =
            readKeysAndThen
                [ "v", "V", "<c-v>" ]
                (\key1 -> [ PushKeys [ key, key1 ] ])
                (\key1 ->
                    (P.map
                        ((::) (PushKeys [ key, key1 ]))
                        (operatorRange <| flipInclusive >> map)
                    )
                )

        defineOperator key op opop =
            (if isVisual then
                readKeyAndThen key
                    [ PushKey key
                    , op VisualRange |> PushOperator
                    , PushComplete
                    , PopMode
                    ]
                <|
                    P.succeed [ PushKey key, PushComplete ]
             else
                readKeyAndThen key [ PushKey key ] <|
                    P.oneOf
                        [ P.succeed
                            [ PushKey key
                            , PushOperator opop
                            , PushComplete
                            ]
                            |. P.symbol key
                        , operatorVisualRange key op
                        , P.map ((::) (PushKey key)) (operatorRange op)
                        ]
            )
                |> completeAndThen
                    (\modeDelta ->
                        if key == "c" then
                            startInsert
                                |> P.map ((++) (popComplete modeDelta))
                                |> completeAndThen popKey
                        else
                            popKey modeDelta
                    )

        countPrefix =
            countParser
                |> P.andThen
                    (\cnt ->
                        let
                            modeDelta =
                                [ PushCount cnt
                                , PushKey (toString cnt)
                                ]
                        in
                            (P.oneOf
                                [ operator isVisual
                                    |> P.map ((++) modeDelta)
                                    |> completeAndThen popKey
                                , P.map (always modeDelta) P.end
                                , P.succeed []
                                ]
                            )
                    )

        registerPrefix =
            registerParser
                |> P.andThen
                    (\modeDelta ->
                        (P.oneOf
                            [ P.map ((++) modeDelta)
                                (completeAndThen popKey <|
                                    operator isVisual
                                )
                            , P.succeed modeDelta
                            ]
                        )
                    )
    in
        P.oneOf
            [ (if isVisual then
                textObject Select
               else
                P.oneOf []
              )
            , motion Move (P.oneOf [])
            , registerPrefix
            , countPrefix
            , defineInsert "i" []
            , defineInsert "I"
                [ ByClass
                    { class = LineStart
                    , direction = Backward
                    }
                    |> Move
                    |> PushOperator
                ]
            , defineInsert "a"
                [ ByClass
                    { class = CharStart
                    , direction = Forward
                    }
                    |> Move
                    |> PushOperator
                ]
            , defineInsert "A"
                [ ByClass
                    { class = LineEnd
                    , direction = Forward
                    }
                    |> Move
                    |> PushOperator
                ]
            , defineInsert "C"
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
            , defineInsert "s"
                [ (MotionRange Exclusive
                    (ByClass { class = CharStart, direction = Forward })
                  )
                    |> Delete
                    |> PushOperator
                ]
            , defineInsert "S"
                [ TextObject Line False |> Delete |> PushOperator ]
            , defineInsert "o" [ OpenNewLine Forward |> PushOperator ]
            , defineInsert "O" [ OpenNewLine Backward |> PushOperator ]
            , defineOperator "d"
                Delete
                (Delete <| TextObject Line True)
            , defineOperator "c"
                Delete
                (Delete <| TextObject Line False)
            , defineOperator "y"
                Yank
                (Yank <| TextObject Line True)
            , defineOperator ">"
                (Indent Forward)
                (Indent Forward <| TextObject Line False)
            , defineOperator "\\<"
                (Indent Backward)
                (Indent Backward <| TextObject Line False)
            , readKeyAndThen ":"
                [ PushKey ":", PushMode <| ModeNameEx ":" ]
                (linebuffer ":" identity)
            , readKeyAndThen "@"
                [ PushKey "@" ]
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
                    ModeNameVisual VisualName

                "V" ->
                    ModeNameVisual VisualNameLine

                "<c-v>" ->
                    ModeNameVisual VisualNameBlock

                _ ->
                    ModeNameVisual VisualName
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
                            |= (operator True
                                    |> completeAndThen
                                        (popComplete >> popKey)
                               )
                        ]
                )


macro : Bool -> Parser ModeDelta
macro isVisual =
    readKeyAndThen "q"
        [ PushKey "q" ]
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
                            , (operator isVisual
                                |> completeAndThen
                                    (popComplete >> popKey)
                              )
                            , P.succeed []
                            ]
                        )
                )
        )


parse : String -> Key -> ( AST, String )
parse lastKeys key =
    if key == "<c-c>" then
        ( initialMode, "" )
    else
        let
            keys =
                lastKeys ++ escapeKey key

            modeDelta =
                P.run (completeAndThen popKey <| operator False) keys
                    |> Result.withDefault []
        in
            ( { count = aggregateCount modeDelta
              , edit = aggregateOperator modeDelta
              , register = aggregateRegister modeDelta
              , modeName = aggregateModeName modeDelta
              , recordMacro = aggregateRecordingMacro modeDelta
              }
            , aggregateKeys modeDelta
            )
