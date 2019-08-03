module Model.Lint exposing (BufferLint, LintError, TextFragment, TextSpan(..), TextWithStyle)

import Internal.Position exposing (..)


type alias TextWithStyle =
    { bold : Bool
    , color : Maybe String
    , underline : Bool
    , string : String
    }


type TextSpan
    = PlainText String
    | RichText TextWithStyle


type alias TextFragment =
    List TextSpan


type alias LintError =
    { tipe : String
    , tag : Maybe String
    , file : String
    , overview : String
    , details : TextFragment
    , region : ( Position, Position )
    , subRegion : Maybe ( Position, Position )
    }


type alias BufferLint =
    { items : List LintError
    , count : Int
    }
