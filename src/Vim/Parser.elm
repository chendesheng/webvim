module Vim.Parser exposing (..)

import Vim.AST exposing (..)
import Vim.Helper exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Vim.Register exposing (..)
import Regex as Re
import Helper.Helper exposing (regex)


-- This parser is crazy, too much edge cases,
--    start feeling that I should just list all results by hand


mapHead : (a -> a) -> List a -> List a
mapHead map items =
    case items of
        head :: tail ->
            map head :: tail

        _ ->
            items


keyToChar : String -> Maybe String
keyToChar key =
    case key of
        "<enter>" ->
            Just "\n"

        "<tab>" ->
            Just "\t"

        "<space>" ->
            Just " "

        _ ->
            if String.length key > 2 then
                Nothing
            else
                Just key


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
            , define "<c-p>" (SelectAutoComplete False)
            , define "<c-n>" (SelectAutoComplete True)
            , P.succeed
                [ PopMode
                , PushComplete
                , PushKey "<inserts><escape>"
                , PopKey
                ]
                |. P.symbol "<escape>"
            , P.succeed
                (\key ->
                    if key == "<inserts>" then
                        [ PushOperator (InsertString LastSavedString) ]
                    else
                        case keyToChar key of
                            Just ch ->
                                [ TextLiteral ch
                                    |> InsertString
                                    |> PushOperator
                                ]

                            _ ->
                                []
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
                (registerKeyEnd <|
                    \key ->
                        [ PushRegister key
                        , Put False |> PushOperator
                        ]
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
                    , PushKey (prefix ++ "<exbuf>" ++ "<enter>")
                    , PopKey
                    , PauseRecording
                    , PushKey (prefix ++ "<enter>")
                    , ContinueRecording
                    , PushComplete
                    ]
                    |. P.symbol "<enter>"
                , P.succeed
                    [ PushComplete, PushEscape ]
                    |. P.symbol "<escape>"
                , P.succeed
                    ((++)
                        [ PushKey prefix
                        , PushMode <| ModeNameEx prefix
                        ]
                    )
                    |= P.oneOf
                        [ P.succeed
                            [ PushOperator <| SelectAutoComplete True ]
                            |. P.symbol "<tab>"
                        , P.succeed
                            [ PushOperator <| SelectAutoComplete False ]
                            |. P.symbol "<s-tab>"
                        , readKeyAndThen "<c-r>"
                            [ PushKey "<c-r>" ]
                            (P.oneOf
                                [ P.succeed
                                    [ PushOperator <|
                                        InsertString WordUnderCursor
                                    ]
                                    |. P.symbol "<c-w>"
                                , registerKeyEnd <|
                                    \key ->
                                        [ PushRegister key
                                        , Put False
                                            |> PushOperator
                                        ]
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
        define pair obj around =
            P.succeed
                [ map obj around |> PushOperator
                , PushKey
                    (if around then
                        "a" ++ pair
                     else
                        "i" ++ pair
                    )
                , PushComplete
                ]
                |. P.symbol pair

        objects around =
            P.oneOf
                [ define "(" (Pair '(') around
                , define ")" (Pair '(') around
                , define "w" Word around
                , define "W" WORD around
                , define "{" (Pair '{') around
                , define "}" (Pair '{') around
                , define "[" (Pair '[') around
                , define "]" (Pair '[') around
                , define "b" (Pair '(') around
                , define "B" (Pair '{') around
                , define "\\<" (Pair '<') around
                , define ">" (Pair '<') around
                , define "t" (Pair 't') around
                , define "\"" (Quote '"') around
                , define "'" (Quote '\'') around
                , define "`" (Quote '`') around
                ]
    in
        P.succeed identity
            |= P.oneOf
                [ readKeyAndThen "i"
                    [ PushKey "i" ]
                    (objects False)
                , readKeyAndThen "a"
                    [ PushKey "a" ]
                    (objects True)
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
                [ define "g" BufferTop gotoLineOption
                    |> dontRecord
                , define "j" VLineDelta (motionOption ">]+=")
                    |> dontRecord
                , define "k" VLineDelta (motionOption "<]+=")
                    |> dontRecord
                , define "n" (MatchString LastSavedString) (motionOption ">]+-")
                    |> dontRecord
                , define "N" (MatchString LastSavedString) (motionOption "<]+-")
                    |> dontRecord
                , define "e" WordEnd backwardWordEndOption
                    |> dontRecord
                , define "E" WORDEnd backwardWordEndOption
                    |> dontRecord
                , extra
                , P.succeed (makePushKeys "g" >> pushComplete)
                    |= keyParser
                ]
            )


gOperator : Parser ModeDelta
gOperator =
    let
        define key op =
            P.succeed
                [ PushKey ("g" ++ key)
                , op
                , PushComplete
                ]
                |. P.symbol key

        defineCaseOperator key changeCase =
            readKeyAndThen key
                [ PushKey <| "g" ++ key ]
                (P.oneOf
                    [ (CaseOperator changeCase
                        |> operatorVisualRange key
                        |> P.map
                            (mapHead
                                (\item ->
                                    case item of
                                        PushKey gkey ->
                                            PushKey ("g" ++ gkey)

                                        _ ->
                                            item
                                )
                            )
                      )
                    , (CaseOperator changeCase
                        |> operatorRange key
                        |> P.map ((::) (PushKey <| "g" ++ key))
                      )
                    ]
                )
                |> completeAndThen
                    (\modeDelta ->
                        let
                            escaped =
                                isEscaped modeDelta
                        in
                            if escaped then
                                modeDelta
                                    |> popKey
                                    |> dontRecord
                            else
                                popKey modeDelta
                    )
    in
        gKey Move <|
            P.oneOf
                [ define "J"
                    (PushOperator <| Join False)
                , define "h"
                    (PushOperator ToggleTip)
                    |> dontRecord
                , define "f"
                    (PushOperator JumpToFile)
                , defineCaseOperator "u" LowerCase
                , defineCaseOperator "U" UpperCase
                , defineCaseOperator "~" SwapCase
                ]


motion :
    Bool
    -> (MotionData -> MotionOption -> Operator)
    -> Parser ModeDelta
    -> Parser ModeDelta
motion isVisual map gMotion =
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
                    , (linebuffer
                        prefix
                        (\cmd ->
                            let
                                option =
                                    motionOption ">)+-"
                            in
                                map (MatchString LastSavedString)
                                    { option | forward = prefix == "/" }
                        )
                      )
                        |> P.map
                            (\changes ->
                                if isVisual && isEscaped changes then
                                    popComplete changes
                                else
                                    changes
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
             , define "j" LineDelta <| motionOption ">]+="
             , define "k" LineDelta <| motionOption "<]+="
             , define "l" CharStart <| motionOption ">)$-"
             , define "^" LineFirst <| motionOption "<)$-"
             , define "0" LineStart <| motionOption "<)$-"
             , define "$" LineEnd <| motionOption ">]$-"
             , define "G" BufferBottom <| motionOption ">]+="
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
             , define "{" Paragraph <| motionOption "<)+-"
             , define "}" Paragraph <| motionOption ">)+-"
             , define ";" RepeatMatchChar <| motionOption ">]$-"
             , define "," RepeatMatchChar <| motionOption "<]$-"
             , define "n" (MatchString LastSavedString) <| motionOption ">)+-"
             , define "N" (MatchString LastSavedString) <| motionOption "<)+-"
             , define "*" (MatchString WordUnderCursor) <| motionOption ">)+-"
             , define "#" (MatchString WordUnderCursor) <| motionOption "<)+-"
             , define "<enter>" NextLineFirst <| motionOption ">]+="
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
            motion False
                toOperator
                (gKey toOperator <|
                    -- gugu
                    if key == "u" || key == "U" || key == "~" then
                        (P.succeed
                            [ TextObject Line True
                                |> map
                                |> PushOperator
                            , PushComplete
                            ]
                            |. P.symbol key
                        )
                    else
                        P.oneOf []
                )

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


operatorVisualRange :
    Key
    -> (OperatorRange -> Operator)
    -> Parser ModeDelta
operatorVisualRange key map =
    readKeysAndThen
        [ "v", "V", "<c-v>" ]
        (\key1 -> [ PushKey (key ++ key1) ])
        (\key1 ->
            (P.map
                ((::) (PushKey (key ++ key1)))
                (operatorRange key <| (visualAfterOperator key1) >> map)
            )
        )


operator : Bool -> Bool -> Parser ModeDelta
operator isVisual isTemp =
    let
        defineHelper mapKey key op =
            (P.succeed
                [ PushOperator op
                , PushKey (mapKey key)
                , PushComplete
                ]
                |. P.symbol key
            )

        define =
            defineHelper identity

        ignoreKey key =
            (P.succeed []
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

        defineOperator key op opop =
            (if isVisual then
                P.succeed
                    (\changes ->
                        (PushKey key) :: changes ++ [ PushComplete ]
                    )
                    |. P.symbol key
                    |= P.oneOf
                        [ P.succeed
                            [ op (VisualRange False) |> PushOperator ]
                            |. P.end
                        , P.succeed []
                        ]
             else
                readKeyAndThen key [ PushKey key ] <|
                    P.oneOf
                        [ P.succeed
                            [ PushKey (key ++ key)
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
                                , PushKey (String.fromInt cnt)
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
            readKeyAndThen "\""
                [ PushKey "\"" ]
                (registerKeyEnd <|
                    \key ->
                        [ PushKey ("\"" ++ key), PushRegister key ]
                )
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

        definez =
            defineHelper ((++) "z")

        jumps =
            P.oneOf
                [ define "<c-u>" (JumpByView -0.5)
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
                , readKeyAndThen "z"
                    [ PushKey "z" ]
                    (P.oneOf
                        [ definez "z" <| Scroll ScrollToMiddle
                        , definez "b" <| Scroll ScrollToBottom
                        , definez "t" <| Scroll ScrollToTop
                        ]
                    )
                    |> dontRecord
                ]

        visualMotion =
            P.oneOf
                [ textObject Select
                , motion True Move (gKey Move <| P.oneOf [])
                ]

        replace =
            readKeyAndThen "r"
                [ PushKey "r" ]
                (P.map
                    (\key ->
                        case keyToChar key of
                            Just ch ->
                                [ PushOperator <| Replace ch
                                , PushKey ("r" ++ ch)
                                , PushComplete
                                ]

                            _ ->
                                []
                    )
                    keyParser
                )
    in
        P.oneOf
            ((if isVisual then
                [ visualMotion
                    |> completeAndThen (popComplete >> popKey)
                , jumps
                    |> completeAndThen (popComplete >> popKey)
                , ignoreKey "<c-o>"
                , ignoreKey "<tab>"
                , P.succeed [ PushOperator VisualSwitchEnd ]
                    |. P.symbol "o"
                , P.succeed [ PushOperator VisualSwitchEnd ]
                    |. P.symbol "O"
                , define "D"
                    (VisualRange True |> Delete)
                , define "x"
                    (VisualRange False |> Delete)
                , define "X"
                    (VisualRange True |> Delete)
                , defineInsert "C"
                    [ VisualRange True
                        |> Delete
                        |> PushOperator
                    ]
                , defineInsert "s"
                    [ VisualRange False
                        |> Delete
                        |> PushOperator
                    ]
                , defineInsert "I"
                    [ PushOperator <| ColumnInsert True ]
                , defineInsert "A"
                    [ PushOperator <| ColumnInsert False ]
                , replace
                , define "u"
                    (VisualRange False |> CaseOperator LowerCase)
                , define "U"
                    (VisualRange False |> CaseOperator UpperCase)
                , define "~"
                    (VisualRange False |> CaseOperator SwapCase)
                ]
              else
                [ defineInsert "i" []
                , motion False Move (P.oneOf [])
                    |> dontRecord
                , defineInsert "a"
                    [ motionOption ">)$-"
                        |> Move CharStart
                        |> PushOperator
                    ]
                , defineInsert "S"
                    [ TextObject Line False |> Delete |> PushOperator ]
                , defineInsert "o" [ OpenNewLine True |> PushOperator ]
                , defineInsert "O" [ OpenNewLine False |> PushOperator ]
                , jumps
                , define "<c-o>" (JumpHistory False)
                    |> dontRecord
                , define "<tab>" (JumpHistory True)
                    |> dontRecord
                , define "D"
                    (motionOption ">)$-"
                        |> MotionRange LineEnd
                        |> Delete
                    )
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
                , defineInsert "C"
                    [ motionOption ">)$-"
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
                , define "<c-^>" JumpLastBuffer
                    |> dontRecord
                , define "<c-a>" (IncreaseNumber True)
                , define "<c-x>" (IncreaseNumber False)
                , define "<c-]>" JumpToTag
                    |> dontRecord
                , define "<c-t>" JumpBackFromTag
                    |> dontRecord
                , define "<c-g>" ShowInfo
                    |> dontRecord
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
                        (Indent True)
                        (Indent True <| TextObject Line False)
                   , defineOperator "\\<"
                        (Indent False)
                        (Indent False <| TextObject Line False)
                   , readKeyAndThen ":"
                        [ PushKey ":", PushMode <| ModeNameEx ":" ]
                        (linebuffer ":" identity)
                        |> dontRecord
                   , readKeyAndThen "@"
                        [ PushKey "@" ]
                        (registerKeyEnd <|
                            \key ->
                                [ PushKey ("@" ++ key)
                                , ReplayMacro key |> PushOperator
                                , PushComplete
                                ]
                        )
                        |> dontRecord
                   , define "J" (Join True)
                   , define "p" (Put True)
                   , define "P" (Put False)
                   , replace
                   , define "." RepeatLastOperator
                        |> dontRecord
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
            |> P.getChompedString
            |> P.andThen
                (\key ->
                    P.oneOf
                        [ P.succeed
                            (\key1 ->
                                if key1 == key || key1 == "<escape>" then
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
                                    , P.symbol "<escape>"
                                    , P.end
                                    ]
                                    |> P.getChompedString
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
        (registerKey
            (P.problem "unknown register")
            (\key ->
                [ PauseRecording
                , PushKey ("q" ++ key)
                , PushRecordMacro key
                , ContinueRecording
                ]
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


containsOnlyDigits : String -> Bool
containsOnlyDigits =
    Re.findAtMost 1 (regex "[^\\d]")
        >> List.isEmpty


parse : String -> Key -> ( AST, String )
parse lastKeys key =
    if key == "<c-c>" then
        ( initialMode, "" )
    else
        let
            keys =
                (lastKeys
                    ++ escapeKey key
                )

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
                        let
                            keys_ =
                                aggregateRecordKeys modeDelta
                        in
                            if containsOnlyDigits keys_ then
                                ""
                            else
                                keys_
                        --|> Debug.log "recordKeys"
                    else
                        ""
              }
            , continuation
            )
