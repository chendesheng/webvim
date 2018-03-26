module Vim.AST exposing (..)


type alias Key =
    String


type alias Register =
    String


defaultRegister : Register
defaultRegister =
    "\""


type StateChange
    = PopKey
    | PushKey String
    | PushMode ModeName
    | PopMode
    | PushOperator Operator
    | PopOperator
    | PushComplete
    | PopComplete
    | PushRecordMacro Register
    | PopRecordMacro
    | PushRegister Register
    | PushCount Int
    | PopCount
    | PauseRecording
    | ContinueRecording


type alias ModeDelta =
    List StateChange


type TextObject
    = Word
    | Line
    | WORD
    | Pair Char -- '', "", <>, (), {}, [], <tag></tag>


type Direction
    = Forward
    | Backward


type OperatorRange
    = TextObject TextObject Bool
    | MotionRange MotionData MotionOption
    | VisualRange


type ScrollPosition
    = ScrollBy Int
    | ScrollToMiddle
    | ScrollToBottom
    | ScrollToTop


type Operator
    = Move MotionData MotionOption
    | Select TextObject Bool -- visual mode
    | Delete OperatorRange
    | Yank OperatorRange
    | Put Bool
    | Indent Direction OperatorRange
    | Join Bool -- J/gJ
    | RepeatLastAction
    | OpenNewLine Direction
    | JumpHistory Direction
    | JumpByView Float -- factor of view height
    | CenterView
    | Scroll ScrollPosition
    | CompleteWord Direction
    | Undo
    | Redo
    | ReplayMacro Register
    | InsertString StringType
    | RepeatLastInsert
    | RepeatLastVisual
    | RepeatLastOperator
    | VisualSwitchEnd
      -- for line buffer
    | InsertWordUnderCursor
    | ExecuteLine


type StringType
    = TextLiteral String
    | WordUnderCursor
    | CharBelowCursor
    | CharAbroveCursor


type VisualName
    = VisualName
    | VisualNameLine
    | VisualNameBlock


type ModeName
    = ModeNameNormal
    | ModeNameInsert
    | ModeNameTempNormal
    | ModeNameEx String
    | ModeNameVisual VisualName


type alias AST =
    { count : Int -- default 1
    , edit : Maybe Operator
    , register : Register
    , modeName : ModeName
    , recordMacro : Maybe Register
    , recordKeys : String
    }


initialMode : AST
initialMode =
    { count = 1
    , edit = Nothing
    , register = defaultRegister
    , modeName = ModeNameNormal
    , recordMacro = Nothing
    , recordKeys = ""
    }


type alias MotionOption =
    { forward : Bool
    , inclusive : Bool
    , crossLine : Bool
    , linewise : Bool -- expand range to entire line, e.g. dj delete tow lines
    }


motionOption : String -> MotionOption
motionOption s =
    case String.toList s of
        [ a, b, c, d ] ->
            { forward = a == '>' -- forward: >, backward: <
            , inclusive = b == ']' -- inclusive: ], exclusive: )
            , crossLine = c == '+' -- crossLine: +, not crossLine: $
            , linewise = d == '=' -- linewise: =, not linewise: -
            }

        _ ->
            { forward = False
            , inclusive = False
            , crossLine = False
            , linewise = False
            }


type MotionData
    = WordStart
    | WordEnd
    | WORDStart
    | WORDEnd
      {- Special case: "cw" and "cW" are treated like "ce" and "cE"
         if the cursor is on a non-blank.
         This is because "cw" is interpreted as change-word,
         and a word does not include the following white space.
      -}
    | WordEdge -- word start or word end
    | WORDEdge
    | LineFirst -- first non-space char of line
    | LineStart
    | LineEnd
    | ParagraphStart
    | ParagraphEnd
    | CharStart
      -- f|t{char}
    | MatchChar String Bool
      -- /search
    | MatchString
    | ViewTop
    | ViewMiddle
    | ViewBottom
    | VLineDelta Int
    | LineDelta Int
    | LineNumber Int -- negtive means backward from last line
    | MatchPair -- %
    | RepeatMatchChar
    | RepeatMatchString
