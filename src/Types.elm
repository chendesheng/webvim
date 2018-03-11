module Types exposing (..)

{-| declear some basic types
-}


{-| ( row, col )
-}
type alias Position =
    ( Int, Int )


positionAdd : Position -> Position -> Position
positionAdd ( y1, x1 ) ( y2, x2 ) =
    ( y1 + y2, x1 + x2 )


positionNeg : Position -> Position
positionNeg ( y, x ) =
    ( -y, -x )


positionSub : Position -> Position -> Position
positionSub a b =
    b |> positionNeg |> positionAdd a
