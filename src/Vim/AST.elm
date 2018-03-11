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
    | PushKeys (List String)
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


type alias ModeDelta =
    List StateChange


type PositionClass
    = WordStart
    | WordEnd
    | WORDStart
    | WORDEnd
    | LineFirst -- first non-space char of line
    | LineStart
    | LineEnd
    | ParagraphStart
    | ParagraphEnd
    | CharStart


type TextObject
    = Word
    | Line
    | WORD
    | Pair Char -- '', "", <>, (), {}, [], <tag></tag>


type Direction
    = Forward
    | Backward


type Inclusive
    = Inclusive
    | Exclusive


type OperatorRange
    = TextObject TextObject Bool
    | MotionRange Inclusive Motion
    | VisualRange


type Operator
    = Move Motion
    | Select TextObject Bool -- visual mode
    | Delete OperatorRange
    | Yank OperatorRange
    | Put Direction
    | Indent Direction OperatorRange
    | Join Bool -- J/gJ
    | RepeatLastAction
    | StartInsert Direction
    | OpenNewLine Direction
    | Scroll Int
    | JumpHistory Direction
    | JumpByView Float -- factor of view height
    | CompleteWord Direction
    | Undo
    | Redo
    | ReplayMacro Register
    | InsertString String
      -- for line buffer
    | InsertWordUnderCursor
    | ExecuteLine


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
    }


initialMode : AST
initialMode =
    { count = 1
    , edit = Nothing
    , register = defaultRegister
    , modeName = ModeNameNormal
    , recordMacro = Nothing
    }


type Motion
    = ByClass
        { class : PositionClass
        , direction : Direction
        }
    | MatchChar
        { char : String
        , direction : Direction
        , inclusive : Inclusive
        }
      -- f|t{char}
    | MatchString Direction
    | ViewTop
    | ViewMiddle
    | ViewBottom
    | VLineDelta Int
    | LineDelta Int
    | LineNumber Int
    | LastLine
    | MatchPair -- %
