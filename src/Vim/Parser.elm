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

        -- motionOption forward inclusive crossLine linewise =
        deleteCharBackward =
            motionOption "<)+-"
                |> MotionRange CharStart
                |> Delete

        deleteWordBackward =
            motionOption "<)+-"
                |> MotionRange WordStart
                |> Delete
    in
        P.oneOf
            [ define "<c-h>" deleteCharBackward
            , define "<backspace>" deleteCharBackward
            , define "<delete>" deleteCharBackward
            , define "<c-w>" deleteWordBackward
            , define "<c-e>" <| InsertString CharBelowCursor
            , define "<c-y>" <| InsertString CharAbroveCursor
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

                                "<space>" ->
                                    " "

                                _ ->
                                    ch
                    in
                        if String.length s > 1 then
                            []
                        else
                            [ TextLiteral s
                                |> InsertString
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
                        in
                            P.succeed
                                (if
                                    (isComplete modeDelta)
                                        || (modeName == ModeNameInsert)
                                 then
                                    List.filter
                                        (\change ->
                                            case change of
                                                PushMode _ ->
                                                    False

                                                PopMode ->
                                                    False

                                                PushKey _ ->
                                                    False

                                                PopKey ->
                                                    False

                                                PushComplete ->
                                                    False

                                                PopComplete ->
                                                    False

                                                _ ->
                                                    True
                                        )
                                        (PopCount :: modeDelta)
                                 else
                                    [ PushMode ModeNameTempNormal
                                    , PushKey "<c-o>"
                                    , PopCount
                                    ]
                                        ++ modeDelta
                                )
                    )
                    (P.lazy (\_ -> operator False True))
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
        P.oneOf
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
    (MotionData -> MotionOption -> Operator)
    -> Parser ModeDelta
    -> Parser ModeDelta
gKey map extra =
    let
        define key md mo =
            P.succeed
                [ PushOperator <| map md mo
                , PushKeys [ "g", key ]
                , PushComplete
                ]
                |. P.symbol key

        -- motionOption forward inclusive crossLine linewise =
        gotoLineOption =
            motionOption ">)+="

        backwardWordEndOption =
            motionOption "<]+-"
    in
        readKeyAndThen "g"
            [ PushKey "g" ]
            (P.oneOf
                [ P.oneOf
                    [ define "g" (LineNumber 0) gotoLineOption
                    , define "j" (VLineDelta 1) gotoLineOption
                    , define "k" (VLineDelta -1) gotoLineOption
                    , define "n" MatchString (motionOption ">]+-")
                    , define "N" MatchString (motionOption "<]+-")
                    , define "e" WordEnd backwardWordEndOption
                    , define "E" WORDEnd backwardWordEndOption
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
    (MotionData -> MotionOption -> Operator)
    -> Parser ModeDelta
    -> Parser ModeDelta
motion map gMotion =
    let
        matchChar trigger forward inclusive =
            readKeyAndThen trigger
                [ PushKey trigger ]
                (P.succeed
                    (\ch ->
                        [ map
                            (MatchChar ch)
                            { forward = forward
                            , inclusive = inclusive
                            , crossLine = False
                            , linewise = False
                            }
                            |> PushOperator
                        , PushKeys [ trigger, ch ]
                        , PushComplete
                        ]
                    )
                    |= keyParser
                )

        define ch md mo =
            P.succeed [ map md mo |> PushOperator, PushKey ch, PushComplete ]
                |. P.symbol ch

        matchString prefix =
            readKeyAndThen prefix
                [ PushKey prefix, PushMode <| ModeNameEx prefix ]
                (linebuffer prefix
                    (\cmd ->
                        if cmd == ExecuteLine then
                            let
                                option =
                                    motionOption ">)+-"
                            in
                                map MatchString
                                    { option | forward = prefix == "/" }
                        else
                            cmd
                    )
                )
    in
        P.oneOf
            ([ define "b" WordStart <| motionOption "<)+-"
             , define "B" WORDStart <| motionOption "<)+-"
             , define "w" WordStart <| motionOption ">)+-"
             , define "W" WORDStart <| motionOption ">)+-"
             , define "e" WordEnd <| motionOption ">]+-"
             , define "E" WORDEnd <| motionOption ">]+-"
             , define "h" CharStart <| motionOption "<)$-"
             , define "j" (LineDelta 1) <| motionOption ">]+="
             , define "k" (LineDelta -1) <| motionOption "<]+="
             , define "l" CharStart <| motionOption ">)$-"
             , define "^" LineFirst <| motionOption "<)$-"
             , define "0" LineStart <| motionOption "<)$-"
             , define "$" LineEnd <| motionOption ">]$-"
             , define "G" (LineNumber -1) <| motionOption ">]+="
             , matchChar "f" True True
             , matchChar "F" False True
             , matchChar "t" True False
             , matchChar "T" False False
             , define "H" ViewTop <| motionOption "<]+="
             , define "M" ViewMiddle <| motionOption "<]+="
             , define "L" ViewBottom <| motionOption "<]+="
             , define "%" MatchPair <| motionOption "<]+-"
             , matchString "/"
             , matchString "?"
             , define "{" ParagraphEnd <| motionOption "<)+-"
             , define "}" ParagraphStart <| motionOption ">)+-"
             , gMotion
             ]
            )


operator : Bool -> Bool -> Parser ModeDelta
operator isVisual isTemp =
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
                , (P.succeed ((++) [ PopOperator, PushMode ModeNameInsert ])
                    |= insertMode
                  )
                ]

        defineInsert key op =
            (P.succeed identity
                |. P.symbol key
                |= startInsert
            )
                |> P.map ((++) (op ++ [ PushKey key ]))
                |> completeAndThen popKey

        operatorRange : Bool -> (OperatorRange -> Operator) -> Parser ModeDelta
        operatorRange isChangeOperator map =
            let
                toOperator md mo =
                    MotionRange md mo |> map

                define ch md mo =
                    P.succeed
                        [ toOperator md mo
                            |> PushOperator
                        , PushKey ch
                        , PushComplete
                        ]
                        |. P.symbol ch

                motionParser =
                    motion
                        toOperator
                        (gKey toOperator <| P.oneOf [])

                textObjectParser =
                    textObject
                        (\obj around ->
                            TextObject obj around |> map
                        )
            in
                if isChangeOperator then
                    P.oneOf
                        -- w/W behavior differently in change operator
                        [ define "w" WordEdge <| motionOption ">]$-"
                        , define "W" WORDEdge <| motionOption ">]$-"
                        , textObjectParser
                        , motionParser
                        ]
                else
                    P.oneOf
                        [ textObjectParser
                        , motionParser
                        ]

        operatorVisualRange key map =
            readKeysAndThen
                [ "v", "V", "<c-v>" ]
                (\key1 -> [ PushKeys [ key, key1 ] ])
                (\key1 ->
                    (P.map
                        ((::) (PushKeys [ key, key1 ]))
                        (operatorRange False <| flipInclusive >> map)
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
                        , P.map ((::) (PushKey key))
                            (operatorRange
                                (key == "c")
                                op
                            )
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
                                [ operator isVisual False
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
                                    operator isVisual False
                                )
                            , P.succeed modeDelta
                            ]
                        )
                    )
    in
        P.oneOf
            ([ (if isVisual then
                    textObject Select
                else
                    P.oneOf []
               )
             , motion Move (P.oneOf [])
             , registerPrefix
             , countPrefix
             , defineInsert "i" []
             , defineInsert "I"
                [ motionOption "<)$-"
                    |> Move LineFirst
                    |> PushOperator
                ]
             , defineInsert "a"
                [ motionOption ">)$-"
                    |> Move CharStart
                    |> PushOperator
                ]
             , defineInsert "A"
                [ motionOption ">]$-"
                    |> Move LineEnd
                    |> PushOperator
                ]
             , defineInsert "C"
                [ motionOption ">]$-"
                    |> MotionRange LineEnd
                    |> Delete
                    |> PushOperator
                ]
             , defineInsert "s"
                [ motionOption ">)$-"
                    |> MotionRange CharStart
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
             , define "D"
                (motionOption ">]$-"
                    |> MotionRange LineEnd
                    |> Delete
                )
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
                (motionOption ">)$-"
                    |> MotionRange CharStart
                    |> Delete
                )
             , define "X"
                (motionOption "<)$-"
                    |> MotionRange CharStart
                    |> Delete
                )
             , gOperator
             , P.lazy (\_ -> macro isVisual)
             , P.lazy (\_ -> visual)
             ]
                ++ if isTemp then
                    [ P.map ((++) [ PopMode, PopKey ]) insertMode ]
                   else
                    [ define "u" Undo
                    , define "<c-r>" Redo
                    ]
            )


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
                                    [ key2mode key |> PushMode
                                    , PushKey key
                                    ]
                                else
                                    [ key2mode key1 |> PushMode
                                    , PushKey key1
                                    ]
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
                            |= (operator True False
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
                            , (operator isVisual False
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
                P.run (completeAndThen popKey <| operator False False) keys
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
