module Vim.State exposing (..)


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



--type alias ModeDelta =
--    ( ModeNameDelta, KeysDelta )
-- type KeysDelta
--     = PushKey
--     | PopKey
--     | PopKeys Int
--     | KeepKeys
--     | ClearKeys


type InsertCommand
    = GotoLineStart
    | GotoLineEnd
    | DeleteChar Direction
    | DeleteWord Direction
    | IndentLine Direction
    | InsertString String
    | PutRegister
    | EscapeInsert
    | PartialInsertCommand


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
    | PunctuationStart
    | PunctuationEnd


type TextObject
    = Word
    | Line
    | WORD
    | Pair Char -- '', "", <>, (), {}, [], <tag></tag>
    | PartialTextObject


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
    | InsertMode InsertCommand
    | ExMode ExCommand
    | CompleteWord Direction
    | Undo
    | Redo
    | ReplayMacro Register


type ExCommand
    = ExInsert InsertCommand
    | InsertWordUnderCursor
    | Execute


type ModeName
    = ModeNameNormal
    | ModeNameInsert
    | ModeNameTempNormal
    | ModeNameEx String
    | ModeNameVisual
    | ModeNameVisualLine
    | ModeNameVisualBlock



--type ModeNameDelta
--    = ChangeMode ModeName
--    | KeepMode


type alias Mode =
    { count : Int -- default 1
    , edit : Maybe Operator
    , register : Register
    , mode : ModeName
    , recordMacro : Maybe Register
    }


initialMode : Mode
initialMode =
    { count = 1
    , edit = Nothing
    , register = defaultRegister
    , mode = ModeNameNormal
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
    | PartialMotion


type alias State =
    ( Mode, String )



{-
   ==> "w"
   <== ( Normal
           { operator = Move (ByClass { class = WordStart }) }
       , "w"
       )


   ==> "dw"
   <== ( Normal
            { operator = Delete MotionRange (ByClass { class = WordStart }) }
       , "dw"
       )


   ==> "cw"
   <== ( Insert
            { operator = Change, ... }
            (InsertString "")
       , "cw"
       )

   ==> "ia"
   <== ( Insert
            { ... }
            (InsertString "a")
       , "i"
       )

   ==> "i<esc>"
   <== ( Normal { ... }
       , ""
       )

   ==> "/a"
   <== ( Normal
            { operator = MatchString
                { command = InsertString "a"
                , direction = Forward
                , inclusive = Inclusive
                }
            }
       , "/"
       )

   ==> "d/a"
   <== ( Normal
            { operator = Delete (MotionRange Exclusive MatchString
                { command = InsertString "a"
                , direction = Forward
                , inclusive = Inclusive
                })
            }
       , "d/"
       )

   ==> "d/<cr>"
   <== ( Normal
            { operator = Delete (MotionRange Exclusive MatchString
                { command = Execute
                , direction = Forward
                , inclusive = Inclusive
                })
            }
       , ""
       )

   ==> "vw"
   <== ( Normal
            { operator = Move (ByClass { class = WordStart })
            , visual = Visual
            }
       , "v"
       )

   ==> "v/<cr>"
   <== ( Normal
            { operator = Move (MatchString
                { command = Execute
                , direction = Forward
                , inclusive = True
                })
            , visual = Visual
            }
       , ""
       )

   ==> ":1"
   <== ( ExMode ":" (ExInsert InsertString "1")
       , ":"
       )

   ==> ":<esc>"
   <== ( Normal { ... }
       , ""
       )

   ==> ":<cr>"
   <== ( ExMode ":" Execute
       , ""
       )

-}
