module Vim.Parser exposing (..)

import Vim.AST exposing (..)
import Vim.Helper exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Vim.Register exposing (..)


-- This parser is crazy, too much edge cases,
--    start feeling that I should just list all results by hand


insertCommands : Parser ModeDelta
insertCommands =
    --common insert commands
    let
        define key op =
            (P.succeed
                [ PushOperator <| op
                ]
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
            , define "<c-p>" (CompleteWord Backward)
            , define "<c-n>" (CompleteWord Forward)
            , P.succeed
                [ PopMode
                , PushComplete
                , PushKey "<inserts><esc>"
                , PopKey
                ]
                |. P.symbol "<esc>"
            , P.succeed
                (\ch ->
                    if ch == "<inserts>" then
                        [ RepeatLastInsert |> PushOperator ]
                    else
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
                [ PushKey "<c-o>"
                , PushMode ModeNameTempNormal
                ]
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
                            , Put False |> PushOperator
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
        P.succeed identity
            |= P.oneOf
                [ P.succeed
                    [ Execute |> map |> PushOperator
                    , PushKey (prefix ++ "<exbuf>" ++ "<cr>")
                    , PopKey
                    , PauseRecording
                    , PushKey (prefix ++ "<cr>")
                    , ContinueRecording
                    , PushComplete
                    ]
                    |. P.symbol "<cr>"
                , P.succeed
                    [ PushComplete, PushEscape ]
                    |. P.symbol "<esc>"
                , P.succeed
                    ((++)
                        [ PushKey prefix
                        , PushMode <| ModeNameEx prefix
                        ]
                    )
                    |= P.oneOf
                        [ readKeyAndThen "<c-r>"
                            [ PushKey "<c-r>" ]
                            (P.oneOf
                                [ P.succeed
                                    [ PushOperator InsertWordUnderCursor ]
                                    |. P.symbol "<c-w>"
                                , P.succeed
                                    (\key ->
                                        if isRegister key then
                                            [ PushRegister key
                                            , Put False
                                                |> PushOperator
                                            ]
                                        else
                                            []
                                    )
                                    |= keyParser
                                ]
                            )
                        , P.succeed [ PushOperator RepeatLastEx ]
                            |. P.symbol "<exbuf>"
                        , insertCommands
                        ]
                ]


textObject : (TextObject -> Bool -> Operator) -> Parser ModeDelta
textObject map =
    let
        define pair obj =
            P.succeed identity
                |= P.oneOf
                    [ readKeyAndThen "i"
                        [ PushKey "i" ]
                        (P.succeed
                            [ map obj False |> PushOperator
                            , PushKey ("i" ++ pair)
                            , PushComplete
                            ]
                            |. P.symbol pair
                        )
                    , readKeyAndThen "a"
                        [ PushKey "a" ]
                        (P.succeed
                            [ map obj True |> PushOperator
                            , PushKey ("a" ++ pair)
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
                , PushKey ("g" ++ key)
                , PushComplete
                ]
                |. P.symbol key

        -- motionOption forward inclusive crossLine linewise =
        gotoLineOption =
            motionOption ">]+="

        backwardWordEndOption =
            motionOption "<]+-"
    in
        readKeyAndThen "g"
            [ PushKey "g" ]
            (P.oneOf
                [ P.oneOf
                    [ define "g" (LineNumber 0) gotoLineOption
                    , define "j" (VLineDelta 1) gotoLineOption
                        |> dontRecord
                    , define "k" (VLineDelta -1) gotoLineOption
                        |> dontRecord
                    , define "n" MatchString (motionOption ">]+-")
                        |> dontRecord
                    , define "N" MatchString (motionOption "<]+-")
                        |> dontRecord
                    , define "e" WordEnd backwardWordEndOption
                        |> dontRecord
                    , define "E" WORDEnd backwardWordEndOption
                        |> dontRecord
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
                    , PushKey "gJ"
                    , PushComplete
                    ]
                ]


motion :
    (MotionData -> MotionOption -> Operator)
    -> Parser ModeDelta
    -> Parser ModeDelta
motion map gMotion =
    let
        matchChar trigger forward before =
            readKeyAndThen trigger
                [ PushKey trigger ]
                (P.succeed
                    (\ch ->
                        [ map
                            (MatchChar ch before)
                            { forward = forward
                            , inclusive = True
                            , crossLine = False
                            , linewise = False
                            }
                            |> PushOperator
                        , PushKey (trigger ++ ch)
                        , PushComplete
                        ]
                    )
                    |= keyParser
                )

        define ch md mo =
            P.succeed
                [ map md mo |> PushOperator
                , PushKey ch
                , PushComplete
                ]
                |. P.symbol ch

        matchString prefix =
            P.succeed identity
                |. P.symbol prefix
                |= P.oneOf
                    [ P.succeed
                        [ PushKey prefix
                        , PushMode <| ModeNameEx prefix
                        ]
                        |. P.end
                    , (linebuffer prefix
                        (\cmd ->
                            let
                                option =
                                    motionOption ">)+-"
                            in
                                map MatchString
                                    { option | forward = prefix == "/" }
                        )
                      )
                    ]
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
             , matchChar "f" True False
             , matchChar "F" False False
             , matchChar "t" True True
             , matchChar "T" False True
             , define "H" ViewTop <| motionOption "<]+="
             , define "M" ViewMiddle <| motionOption "<]+="
             , define "L" ViewBottom <| motionOption "<]+="
             , define "%" MatchPair <| motionOption "<]+-"
             , matchString "/"
             , matchString "?"
             , define "{" ParagraphEnd <| motionOption "<)+-"
             , define "}" ParagraphStart <| motionOption ">)+-"
             , define ";" RepeatMatchChar <| motionOption ">]$-"
             , define "," RepeatMatchChar <| motionOption "<]$-"
             , define "n" RepeatMatchString <| motionOption ">)+-"
             , define "N" RepeatMatchString <| motionOption "<)+-"
             , gMotion
             ]
            )



--recordKey : Parser ModeDelta -> Parser ModeDelta
--recordKey =
--    P.sourceMap
--        (\s changes ->
--            if isComplete changes then
--                RecordKey s :: changes
--            else
--                changes
--        )
--
--
--recordKeyFinish : Parser ModeDelta -> Parser ModeDelta
--recordKeyFinish =
--    P.sourceMap
--        (\s changes ->
--            RecordKeyFinish s :: changes
--        )
--
--


dontRecord : Parser ModeDelta -> Parser ModeDelta
dontRecord =
    P.map
        (\changes ->
            PauseRecording :: changes ++ [ ContinueRecording ]
        )


alwaysRecord : Parser ModeDelta -> Parser ModeDelta
alwaysRecord =
    P.map
        (\changes ->
            List.filter
                (\change ->
                    case change of
                        PauseRecording ->
                            False

                        ContinueRecording ->
                            False

                        _ ->
                            True
                )
                changes
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

        startInsert key =
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
                |= startInsert key
            )
                |> P.map ((++) (op ++ [ PushKey key ]))
                |> completeAndThen popKey

        operatorRange :
            Key
            -> (OperatorRange -> Operator)
            -> Parser ModeDelta
        operatorRange key map =
            let
                toOperator md mo =
                    MotionRange md mo |> map

                define ch md mo =
                    P.succeed
                        ([ toOperator md mo
                            |> PushOperator
                         , PushKey ch
                         , PushComplete
                         ]
                        )
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
                if key == "c" then
                    P.oneOf
                        -- w/W behavior differently in change operator
                        [ define "w" WordEdge <| motionOption ">)$-"
                        , define "W" WORDEdge <| motionOption ">)$-"
                        , textObjectParser
                        , motionParser
                        ]
                else
                    P.oneOf
                        -- w/W behavior differently in change operator
                        [ define "w" WordStart <| motionOption ">)$-"
                        , define "W" WORDStart <| motionOption ">)$-"
                        , textObjectParser
                        , motionParser
                        ]

        operatorVisualRange key map =
            readKeysAndThen
                [ "v", "V", "<c-v>" ]
                (\key1 -> [ PushKey (key ++ key1) ])
                (\key1 ->
                    (P.map
                        ((::) (PushKey (key ++ key1)))
                        (operatorRange key <| flipInclusive >> map)
                    )
                )

        defineOperator key op opop =
            (if isVisual then
                P.succeed
                    (\changes -> (PushKey key) :: changes ++ [ PushComplete ])
                    |. P.symbol key
                    |= P.oneOf
                        [ P.succeed
                            [ op VisualRange |> PushOperator ]
                            |. P.end
                        , P.succeed []
                        ]
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
                            (operatorRange key op)
                        ]
            )
                |> completeAndThen
                    (\modeDelta ->
                        let
                            escaped =
                                isEscaped modeDelta
                        in
                            if key == "c" && not escaped then
                                startInsert key
                                    |> P.map ((++) (popComplete modeDelta))
                                    |> completeAndThen popKey
                            else if escaped then
                                modeDelta
                                    |> popKey
                                    |> dontRecord
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
                                ((if isVisual then
                                    [ visualMotion
                                        |> P.map ((++) modeDelta)
                                        |> completeAndThen
                                            (\changes ->
                                                changes
                                                    ++ [ PopComplete

                                                       --pop count key
                                                       , PopKey

                                                       -- pop motion key
                                                       , PopKey
                                                       ]
                                                    |> P.succeed
                                            )
                                    ]
                                  else
                                    []
                                 )
                                    ++ [ operator isVisual False
                                            |> P.map ((++) modeDelta)
                                            |> completeAndThen popKey
                                       , P.map (always modeDelta) P.end
                                       , P.succeed []
                                       ]
                                )
                            )
                    )

        registerPrefix =
            registerParser
                |> dontRecord
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

        visualMotion =
            P.oneOf
                [ textObject Select
                , motion Move (gKey Move <| P.oneOf [])
                ]
    in
        P.oneOf
            ((if isVisual then
                [ visualMotion
                    |> completeAndThen (popComplete >> popKey)
                , P.succeed [ PushOperator VisualSwitchEnd ]
                    |. P.symbol "o"
                , P.succeed [ PushOperator VisualSwitchEnd ]
                    |. P.symbol "O"
                ]
              else
                [ defineInsert "i" []
                , motion Move (P.oneOf [])
                    |> dontRecord
                , defineInsert "a"
                    [ motionOption ">)$-"
                        |> Move CharStart
                        |> PushOperator
                    ]
                , defineInsert "S"
                    [ TextObject Line False |> Delete |> PushOperator ]
                , defineInsert "o" [ OpenNewLine Forward |> PushOperator ]
                , defineInsert "O" [ OpenNewLine Backward |> PushOperator ]
                ]
             )
                ++ [ countPrefix
                   , registerPrefix
                   , defineInsert "I"
                        [ motionOption "<)$-"
                            |> Move LineFirst
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
                        |> dontRecord
                   , readKeyAndThen "@"
                        [ PushKey "@" ]
                        (P.map
                            (\key ->
                                if isRegister key then
                                    [ PushKey ("@" ++ key)
                                    , ReplayMacro key |> PushOperator
                                    , PushComplete
                                    ]
                                else
                                    []
                            )
                            keyParser
                        )
                        |> dontRecord
                   , define "D"
                        (motionOption ">)$-"
                            |> MotionRange LineEnd
                            |> Delete
                        )
                   , define "<c-o>" (JumpHistory Backward)
                        |> dontRecord
                   , define "<tab>" (JumpHistory Forward)
                        |> dontRecord
                   , define "<c-u>" (JumpByView -0.5)
                        |> dontRecord
                   , define "<c-d>" (JumpByView 0.5)
                        |> dontRecord
                   , define "<c-f>" (JumpByView 1)
                        |> dontRecord
                   , define "<c-b>" (JumpByView -1)
                        |> dontRecord
                   , define "<c-y>" (Scroll <| ScrollBy -1)
                        |> dontRecord
                   , define "<c-e>" (Scroll <| ScrollBy 1)
                        |> dontRecord
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
                   , define "p" (Put True)
                        |> dontRecord
                   , define "P" (Put False)
                        |> dontRecord
                   , define "." RepeatLastOperator
                        |> dontRecord
                   , readKeyAndThen "z"
                        [ PushKey "z" ]
                        (P.oneOf
                            [ define "z" <| Scroll ScrollToMiddle
                            , define "b" <| Scroll ScrollToBottom
                            , define "t" <| Scroll ScrollToTop
                            ]
                        )
                   , gOperator
                   , P.lazy (\_ -> macro isVisual)
                   , P.lazy (\_ -> visual)
                   ]
                ++ if isTemp then
                    [ P.map ((++) [ PopMode, PopKey ]) insertMode ]
                   else
                    [ define "u" Undo
                        |> dontRecord
                    , define "<c-r>" Redo
                        |> dontRecord
                    ]
            )


visual : Parser ModeDelta
visual =
    let
        key2mode key =
            case key of
                "v" ->
                    ModeNameVisual VisualChars

                "V" ->
                    ModeNameVisual VisualLine

                "<c-v>" ->
                    ModeNameVisual VisualBlock

                _ ->
                    ModeNameVisual VisualChars
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
                            |= P.oneOf
                                [ P.succeed [ PushOperator RepeatLastVisual ]
                                    |. P.symbol "<visual>"
                                , (operator True False
                                    |> completeAndThen
                                        (\changes ->
                                            ([ PushKey "<visual>", PopKey ]
                                                ++ changes
                                                ++ [ PopKey
                                                   , PopMode
                                                   ]
                                            )
                                                |> P.succeed
                                        )
                                  )
                                ]
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
                            [ PushKey ("q" ++ key)
                            , PushRecordMacro key
                            ]
                            |> dontRecord
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
                lastKeys
                    ++ escapeKey key

            --|> Debug.log "keys"
            modeDelta =
                P.run (completeAndThen popKey <| operator False False) keys
                    |> Result.withDefault []

            --|> Debug.log "modeDelta"
            continuation =
                aggregateKeys modeDelta
        in
            ( { count = aggregateCount modeDelta
              , edit = aggregateOperator modeDelta
              , register = aggregateRegister modeDelta
              , modeName = aggregateModeName modeDelta
              , recordMacro = aggregateRecordingMacro modeDelta
              , recordKeys =
                    if String.isEmpty continuation then
                        aggregateRecordKeys modeDelta
                        --|> Debug.log "recordKeys"
                    else
                        ""
              }
            , continuation
            )
