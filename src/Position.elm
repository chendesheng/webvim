module Position exposing (..)

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


positionBound : Position -> Position -> Position -> Position
positionBound min max pos =
    if pos < min then
        min
    else if pos > max then
        max
    else
        pos


positionMin : Position -> Position -> Position
positionMin a b =
    if a < b then
        a
    else
        b


positionMax : Position -> Position -> Position
positionMax a b =
    if a < b then
        b
    else
        a



--changeInclusive : Bool -> Bool -> Position -> Position
--changeInclusive from to (( y, x ) as pos) =
--    if from == to then
--        pos
--    else if from then
--        -- inclusive to exclusive
--        ( y, x + 1 )
--    else
--        ( y, x - 1 )
