module Internal.TextBuffer
    exposing
        ( TextBuffer
        , isEmpty
        , lineBreak
        , applyPatch
        , empty
        , fromList
        , fromString
        , getLine
        , slice
        , append
        , (+++)
        , boundPosition
        , countLines
        , foldlLines
        , mapLines
        )

import Types exposing (..)
import Array exposing (Array)
import String


type alias TextBuffer =
    Array String


lineBreak : String
lineBreak =
    "\n"


isEmpty : TextBuffer -> Bool
isEmpty =
    Array.isEmpty


empty : TextBuffer
empty =
    Array.empty


fromList : List String -> TextBuffer
fromList =
    Array.fromList


countLines : TextBuffer -> Int
countLines =
    Array.length


mapLines : (String -> b) -> TextBuffer -> Array b
mapLines =
    Array.map


foldlLines : (String -> a -> a) -> a -> TextBuffer -> a
foldlLines =
    Array.foldl


getLine : Int -> TextBuffer -> Maybe String
getLine =
    Array.get


getLastLine : TextBuffer -> String
getLastLine buf =
    buf
        |> getLine (countLines buf - 1)
        |> Maybe.withDefault ""


getFirstLine : TextBuffer -> String
getFirstLine buf =
    buf
        |> getLine 0
        |> Maybe.withDefault ""


boundPosition : TextBuffer -> Position
boundPosition buf =
    if isEmpty buf then
        ( 0, 0 )
    else
        let
            s =
                getLastLine buf

            cnt =
                countLines buf
        in
            ( cnt - 1, String.length s )


{-| Convert a string to a textBuffer
-}
fromString : String -> TextBuffer
fromString s =
    if s == "" then
        empty
    else
        let
            buf =
                String.split lineBreak s
                    |> fromList

            lastRow =
                countLines buf - 1
        in
            buf
                |> Array.indexedMap
                    (\i s ->
                        if i == lastRow then
                            s
                        else
                            s ++ lineBreak
                    )


toString : TextBuffer -> String
toString =
    Array.toList >> String.join ""


{-| Concat tow textBuffers
-}
append : TextBuffer -> TextBuffer -> TextBuffer
append buf1 buf2 =
    if isEmpty buf1 then
        buf2
    else if isEmpty buf2 then
        buf1
    else
        let
            lastLine1 =
                getLastLine buf1

            firstLine2 =
                getFirstLine buf2
        in
            buf2
                |> Array.set 0 (lastLine1 ++ firstLine2)
                |> Array.append (Array.slice 0 (countLines buf1 - 1) buf1)


{-| slice from a TextBuffer, right side exclusive
-}
slice : Position -> Position -> TextBuffer -> TextBuffer
slice pos1 pos2 buf =
    if pos2 <= pos1 then
        empty
    else
        let
            ( y1, x1 ) =
                if pos1 < ( 0, 0 ) then
                    ( 0, 0 )
                else
                    pos1

            bound =
                boundPosition buf

            ( y2, x2 ) =
                if pos2 > bound then
                    bound
                else
                    pos2
        in
            Maybe.map2
                (\line1 line2 ->
                    if y1 == y2 then
                        String.slice x1 x2 line1
                            |> fromString
                    else if y1 + 1 == y2 then
                        (String.dropLeft x1 line1 ++ String.left x2 line2)
                            |> fromString
                    else
                        let
                            line11 =
                                String.dropLeft x1 line1
                        in
                            (if String.isEmpty line11 then
                                []
                             else
                                [ line11 ]
                            )
                                |> fromList
                                |> flip Array.append
                                    (Array.slice (y1 + 1) y2 buf)
                                |> Array.push (String.left x2 line2)
                )
                (Array.get y1 buf)
                (Array.get y2 buf)
                |> Maybe.withDefault empty


applyInsertion : Position -> String -> TextBuffer -> ( Patch, TextBuffer )
applyInsertion pos s buf =
    let
        ss =
            fromString s

        bound =
            boundPosition buf

        pos1 =
            if pos >= bound then
                bound
            else if pos < ( 0, 0 ) then
                ( 0, 0 )
            else
                let
                    ( y, x ) =
                        pos
                in
                    getLine y buf
                        |> Maybe.map
                            (\line ->
                                if String.length line <= x then
                                    -- because of pos < bound,
                                    -- y is not last line
                                    ( y + 1, 0 )
                                else
                                    pos
                            )
                        -- because of pos < bound, this will never happen
                        |> Maybe.withDefault ( 0, 0 )

        top =
            slice ( 0, 0 ) pos1 buf

        bottom =
            slice pos1 bound buf

        ( y, x ) =
            pos1

        ( dy, dx ) =
            boundPosition ss
    in
        ( Deletion pos1
            ( y + dy
            , dx
                + (if dy == 0 then
                    x
                   else
                    0
                  )
            )
        , top +++ ss +++ bottom
        )


applyDeletion :
    Position
    -> Position
    -> TextBuffer
    -> ( Patch, TextBuffer )
applyDeletion pos1 pos2 buf =
    if pos2 <= pos1 then
        ( Insertion pos1 "", buf )
    else
        let
            pos11 =
                if pos1 < ( 0, 0 ) then
                    ( 0, 0 )
                else
                    pos1

            pos22 =
                if pos2 < ( 0, 0 ) then
                    ( 0, 0 )
                else
                    pos2

            top =
                slice ( 0, 0 ) pos11 buf

            bottom =
                slice pos2 (boundPosition buf) buf

            deleted =
                buf
                    |> slice pos11 pos22
                    |> toString
        in
            ( Insertion pos11 deleted, top +++ bottom )


{-| apply a patch, returns "reversed" patch and result buf
-}
applyPatch : Patch -> Array String -> ( Patch, Array String )
applyPatch patch buf =
    case patch of
        Insertion pos s ->
            applyInsertion pos s buf

        Deletion pos1 pos2 ->
            applyDeletion pos1 pos2 buf


(+++) : TextBuffer -> TextBuffer -> TextBuffer
(+++) =
    append
