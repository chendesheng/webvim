module Types exposing (..)

{-| declear some basic types
-}


{-| ( row, col )
-}
type alias Position =
    ( Int, Int )


type Patch
    = Insertion Position String
      -- Both sides are inclusive
    | Deletion Position Position


emptyPatch : Patch
emptyPatch =
    Insertion ( 0, 0 ) ""


type alias Undo =
    { cursor : Position
    , patch : Patch
    }


type alias Redo =
    Undo
