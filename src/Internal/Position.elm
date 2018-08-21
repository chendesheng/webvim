module Internal.Position exposing (..)

import Json.Decode as Decode


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


positionDecoder : Decode.Decoder Position
positionDecoder =
    Decode.map2 (\a b -> ( a - 1, b - 1 ))
        (Decode.field "line" Decode.int)
        (Decode.field "column" Decode.int)


endPositionDecoder : Decode.Decoder Position
endPositionDecoder =
    Decode.map2 (\a b -> ( a - 1, b - 1 ))
        (Decode.field "endLine" Decode.int)
        (Decode.field "endColumn" Decode.int)


regionDecoder : Decode.Decoder ( Position, Position )
regionDecoder =
    Decode.map2 (,)
        (Decode.field "start" positionDecoder)
        (Decode.field "end" positionDecoder)


excludeRight : ( Position, Position ) -> ( Position, Position )
excludeRight ( p1, ( y, x ) ) =
    ( p1, ( y, max 0 (x - 1) ) )
