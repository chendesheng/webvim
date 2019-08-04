module Vim.AST exposing
    ( AST
    , ChangeCase(..)
    , Direction(..)
    , Key
    , ModeDelta
    , ModeName(..)
    , MotionData(..)
    , MotionOption
    , Operator(..)
    , OperatorRange(..)
    , Register
    , ScrollPosition(..)
    , StateChange(..)
    , StringType(..)
    , SwitchViewType(..)
    , TextObject(..)
    , VisualType(..)
    , defaultRegister
    , emptyMotionOption
    , initialMode
    , isEditingOperator
    , isLineDeltaMotion
    , isVisualRange
    , motionOption
    )


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
    | WORD
    | Line
    | Pair Char -- '', "", <>, (), {}, [], <tag></tag>
    | Quote Char -- ', ", `


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


type ChangeCase
    = LowerCase
    | UpperCase
    | SwapCase


type Operator
    = Move MotionData MotionOption
    | Select TextObject Bool -- visual mode
    | Delete OperatorRange
    | Yank OperatorRange
    | CaseOperator ChangeCase OperatorRange
    | Put Bool
    | Indent Bool OperatorRange
    | Join Bool -- J/gJ collapse spaces if argument is true
    | RepeatLastAction
    | OpenNewLine Bool
    | JumpHistory Bool
    | JumpByView Float -- factor of view height
    | JumpLastBuffer
    | JumpToTag --word under cursor
    | JumpBackFromTag
    | JumpToFile
    | CenterView
    | Scroll ScrollPosition
    | CompleteWord Bool
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
    | SelectAutoComplete Bool
    | SelectHistory Bool
    | IncreaseNumber Bool
    | ColumnInsert Bool -- visual mode I/A
    | ShowInfo
    | IMEToggleActive
    | SwitchView SwitchViewType


isEditingOperator : Operator -> Bool
isEditingOperator op =
    case op of
        Delete _ ->
            True

        InsertString _ ->
            True

        Put _ ->
            True

        _ ->
            False


type SwitchViewType
    = SwitchToNext
    | SwitchToPrev
    | SwitchToRight
    | SwitchToLeft
    | SwitchToTop
    | SwitchToBottom


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


emptyMotionOption : MotionOption
emptyMotionOption =
    { forward = False
    , inclusive = False
    , crossLine = False
    , linewise = False
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
        emptyMotionOption


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
    | Paragraph
    | CharStart
      -- f|t{char}
    | MatchChar String Bool
      -- skip blackslash escaped char
    | QuoteChar Char
      -- /search
    | ViewTop
    | ViewMiddle
    | ViewBottom
    | VLineDelta
    | LineDelta
    | BufferTop
    | BufferBottom
    | MatchPair -- %
    | RepeatMatchChar
    | MatchString StringType
    | NextLineFirst -- <enter>


isLineDeltaMotion : Operator -> Bool
isLineDeltaMotion op =
    case op of
        Move mo _ ->
            case mo of
                LineDelta ->
                    True

                _ ->
                    False

        _ ->
            False
