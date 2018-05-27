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
    | PushEscape
    | PopEscape


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
    | VisualRange Bool


isVisualRange : OperatorRange -> Bool
isVisualRange range =
    case range of
        VisualRange _ ->
            True

        _ ->
            False


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
    | Indent Bool OperatorRange
    | Join Bool -- J/gJ collapse spaces if argument is true
    | RepeatLastAction
    | OpenNewLine Bool
    | JumpHistory Bool
    | JumpByView Float -- factor of view height
    | JumpLastBuffer
    | JumpToTag --word under cursor
    | JumpToFile
    | CenterView
    | Scroll ScrollPosition
    | CompleteWord Direction
    | Undo
    | Redo
    | ReplayMacro Register
    | InsertString StringType
    | RepeatLastEx
    | RepeatLastVisual
    | RepeatLastOperator
    | VisualSwitchEnd
    | Execute -- execute ex command
    | Replace String
    | ToggleTip
    | SelectAutoComplete Direction
    | IncreaseNumber Bool


type StringType
    = TextLiteral String
    | WordUnderCursor
    | CharBelowCursor
    | CharAbroveCursor
    | LastSavedString


type VisualType
    = VisualLine
    | VisualBlock
    | VisualChars


type ModeName
    = ModeNameNormal
    | ModeNameInsert
    | ModeNameTempNormal
    | ModeNameEx String
    | ModeNameVisual VisualType


type alias AST =
    { count : Maybe Int -- default 1
    , edit : Maybe Operator
    , register : Register
    , modeName : ModeName
    , recordMacro : Maybe Register
    , recordKeys : String
    }


initialMode : AST
initialMode =
    { count = Nothing
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
    if s == ">]+=" then
        { forward = True
        , inclusive = True
        , crossLine = True
        , linewise = True
        }
    else if s == "<]+=" then
        { forward = False
        , inclusive = True
        , crossLine = True
        , linewise = True
        }
    else if s == ">)+=" then
        { forward = True
        , inclusive = False
        , crossLine = True
        , linewise = True
        }
    else if s == ">]$=" then
        { forward = True
        , inclusive = True
        , crossLine = False
        , linewise = True
        }
    else if s == ">]+-" then
        { forward = True
        , inclusive = True
        , crossLine = True
        , linewise = False
        }
    else if s == "<)+=" then
        { forward = False
        , inclusive = False
        , crossLine = True
        , linewise = True
        }
    else if s == "<]$=" then
        { forward = False
        , inclusive = True
        , crossLine = False
        , linewise = True
        }
    else if s == "<]+-" then
        { forward = False
        , inclusive = True
        , crossLine = True
        , linewise = False
        }
    else if s == ">)$=" then
        { forward = True
        , inclusive = False
        , crossLine = False
        , linewise = True
        }
    else if s == ">)+-" then
        { forward = True
        , inclusive = False
        , crossLine = True
        , linewise = False
        }
    else if s == ">]$-" then
        { forward = True
        , inclusive = True
        , crossLine = False
        , linewise = False
        }
    else if s == "<)$=" then
        { forward = False
        , inclusive = False
        , crossLine = False
        , linewise = True
        }
    else if s == "<]$-" then
        { forward = False
        , inclusive = True
        , crossLine = False
        , linewise = False
        }
    else if s == "<)+-" then
        { forward = False
        , inclusive = False
        , crossLine = True
        , linewise = False
        }
    else if s == ">)$-" then
        { forward = True
        , inclusive = False
        , crossLine = False
        , linewise = False
        }
    else
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
    | ViewTop
    | ViewMiddle
    | ViewBottom
    | VLineDelta Bool
    | LineDelta Bool
    | BufferTop
    | BufferBottom
    | MatchPair -- %
    | RepeatMatchChar
    | MatchString StringType
