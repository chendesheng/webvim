module Internal.TextBuffer
    exposing
        ( TextBuffer
        , isEmpty
        , lineBreak
        , applyPatch
        , empty
        , fromString
        , fromStringExpandTabs
        , getLine
        , count
        , foldlLines
        , expandTabs
        , mapLines
        , Patch(..)
        , toString
        , lineMaxColumn
        , getLineMaxColumn
        , mapLinesToList
        , indexedMapLinesToList
        , substring
        , sliceLines
        , sliceRegion
        , patchCursor
        , mergePatch
        , shiftPositionByPatch
        , findFirstLine
        )

import Internal.Position exposing (..)
import Elm.Array as Array exposing (Array)
import String
import List


type TextBuffer
    = TextBuffer (Array String)


type Patch
    = Insertion Position TextBuffer
    | Deletion Position Position


patchCursor : Patch -> Position
patchCursor patch =
    case patch of
        Insertion pos _ ->
            pos

        Deletion pos _ ->
            pos


shiftPositionByPatch : Patch -> Position -> Position
shiftPositionByPatch patch pos =
    let
        --_ =
        --Debug.log "patch pos" ( patch, pos, res )
        res =
            case patch of
                Insertion begin (TextBuffer lines) ->
                    if pos < begin then
                        pos
                    else
                        let
                            ( py, px ) =
                                pos

                            ( by, bx ) =
                                begin

                            dy =
                                Array.length lines - 1

                            dx =
                                Array.get dy lines
                                    |> Maybe.map String.length
                                    |> Maybe.withDefault 0
                        in
                            if py == by then
                                ( py + dy, px + dx )
                            else
                                ( py + dy, px )

                Deletion begin end ->
                    if pos < begin then
                        pos
                    else if pos >= end then
                        let
                            ( by, bx ) =
                                begin

                            ( ey, ex ) =
                                end

                            ( py, px ) =
                                pos
                        in
                            if ey == py then
                                if by == ey then
                                    ( py, px - (ex - bx) )
                                else
                                    ( py - (ey - by), px - ex )
                            else
                                ( py - (ey - by), px )
                    else
                        begin
    in
        res


mergePatch : Patch -> Patch -> Maybe Patch
mergePatch p1 p2 =
    let
        add ( y1, x1 ) ( y2, x2 ) =
            if y2 == 0 then
                ( y1, x1 + x2 )
            else
                ( y1 + y2, x2 )
    in
        case p1 of
            Insertion pos1 (TextBuffer lines1) ->
                case p2 of
                    Insertion pos2 (TextBuffer lines2) ->
                        if add pos2 (boundPosition lines2) == pos1 then
                            append lines2 lines1
                                |> TextBuffer
                                |> Insertion pos2
                                |> Just
                        else
                            Nothing

                    _ ->
                        Nothing

            Deletion b1 e1 ->
                case p2 of
                    Deletion b2 e2 ->
                        if b1 == e2 then
                            Just <| Deletion b2 e1
                        else
                            Nothing

                    _ ->
                        Nothing


emptyPatch : Patch
emptyPatch =
    Insertion ( 0, 0 ) empty


lineBreak : String
lineBreak =
    "\n"


isEmpty : TextBuffer -> Bool
isEmpty (TextBuffer buf) =
    isEmptyInner buf


isEmptyInner : Array String -> Bool
isEmptyInner buf =
    Array.get 0 buf
        |> Maybe.map String.isEmpty
        |> Maybe.withDefault True


empty : TextBuffer
empty =
    fromString ""


count : TextBuffer -> Int
count (TextBuffer buf) =
    Array.length buf


mapLines : (String -> b) -> TextBuffer -> Array b
mapLines f (TextBuffer buf) =
    Array.map f buf


mapLinesToList : Int -> Int -> (String -> b) -> TextBuffer -> List b
mapLinesToList begin end f (TextBuffer buf) =
    buf
        |> Array.slice begin end
        |> Array.toList
        |> List.map f


indexedMapLinesToList : Int -> Int -> (Int -> String -> b) -> TextBuffer -> List b
indexedMapLinesToList begin end f (TextBuffer buf) =
    buf
        |> Array.slice begin end
        |> Array.toList
        |> List.indexedMap (\i x -> f (i + begin) x)


foldlLines : Int -> (String -> a -> a) -> a -> TextBuffer -> a
foldlLines n f a (TextBuffer buf) =
    Array.foldl f a <| Array.slice n (Array.length buf) buf


getLine : Int -> TextBuffer -> Maybe String
getLine n (TextBuffer buf) =
    Array.get n buf


getLastLine : Array String -> String
getLastLine buf =
    buf
        |> Array.get (Array.length buf - 1)
        |> Maybe.withDefault ""


getFirstLine : Array String -> String
getFirstLine buf =
    buf
        |> Array.get 0
        |> Maybe.withDefault ""


findFirstLineHelper :
    (String -> Int -> Maybe a)
    -> List String
    -> Int
    -> Maybe a
findFirstLineHelper pred lines i =
    case lines of
        line :: rest ->
            case pred line i of
                Just x ->
                    Just x

                _ ->
                    findFirstLineHelper pred rest (i + 1)

        _ ->
            Nothing


findFirstLine : (String -> Int -> Maybe a) -> TextBuffer -> Maybe a
findFirstLine pred (TextBuffer lines) =
    findFirstLineHelper pred (Array.toList lines) 0



--maxPosition : TextBuffer -> Position
--maxPosition buf =
--    case buf of
--        TextBuffer lines ->
--            let
--                ( y, x ) =
--                    boundPosition lines
--            in
--                ( y, max (x - 1) 0 )


boundPosition : Array String -> Position
boundPosition buf =
    if isEmptyInner buf then
        ( 0, 0 )
    else
        let
            cnt =
                Array.length buf

            s =
                Array.get (cnt - 1) buf
                    |> Maybe.withDefault ""
        in
            ( cnt - 1, String.length s )


{-| Convert a string to a textBuffer
-}
fromString : String -> TextBuffer
fromString =
    fromStringHelper >> TextBuffer


fromStringExpandTabs : Int -> Int -> String -> TextBuffer
fromStringExpandTabs n firstLineOffset s =
    expandTabs n firstLineOffset s
        |> fromStringList
        |> TextBuffer


fromStringHelper : String -> Array String
fromStringHelper s =
    s
        |> String.split lineBreak
        |> fromStringList


fromStringList : List String -> Array String
fromStringList s =
    if List.isEmpty s then
        Array.empty
    else
        let
            buf =
                Array.fromList s

            lastLine =
                Array.length buf - 1
        in
            buf
                |> Array.indexedMap
                    (\i s ->
                        if i == lastLine then
                            s
                        else
                            s ++ lineBreak
                    )


toString : TextBuffer -> String
toString (TextBuffer buf) =
    buf
        |> Array.toList
        |> String.join ""


{-| Concat tow textBuffers
-}
append : Array String -> Array String -> Array String
append buf1 buf2 =
    if isEmptyInner buf1 then
        buf2
    else if isEmptyInner buf2 then
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
                |> Array.append (Array.slice 0 (Array.length buf1 - 1) buf1)


{-| slice from a TextBuffer, right side exclusive
-}
slice : Position -> Position -> Array String -> Array String
slice pos1 pos2 buf =
    let
        bound =
            boundPosition buf

        valid pos =
            if pos < ( 0, 0 ) then
                ( 0, 0 )
            else if pos > bound then
                bound
            else
                pos

        (( y1, x1 ) as pos11) =
            valid pos1

        (( y2, x2 ) as pos22) =
            valid pos2
    in
        if pos22 <= pos11 then
            fromStringHelper ""
        else
            Maybe.map2
                (\line1 line2 ->
                    if y1 == y2 then
                        String.slice x1 x2 line1
                            |> fromStringHelper
                    else if y1 + 1 == y2 then
                        (String.dropLeft x1 line1 ++ String.left x2 line2)
                            |> fromStringHelper
                    else
                        let
                            line11 =
                                String.dropLeft x1 line1

                            line22 =
                                String.left x2 line2
                        in
                            (if String.isEmpty line11 then
                                []
                             else
                                [ line11 ]
                            )
                                |> Array.fromList
                                |> flip Array.append
                                    (Array.slice (y1 + 1) y2 buf)
                                |> (if String.endsWith lineBreak line22 then
                                        (Array.push line22 >> Array.push "")
                                    else
                                        Array.push line22
                                   )
                )
                (Array.get y1 buf)
                (Array.get y2 buf)
                |> Maybe.withDefault Array.empty


applyInsertion :
    Position
    -> TextBuffer
    -> Array String
    -> ( Patch, TextBuffer )
applyInsertion pos (TextBuffer s) buf =
    let
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
                    Array.get y buf
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
                        |> normalizePos buf

        top =
            slice ( 0, 0 ) pos1 buf

        bottom =
            slice pos1 bound buf

        ( y, x ) =
            pos1

        ( dy, dx ) =
            boundPosition s
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
        , top
            +++ s
            +++ bottom
            |> TextBuffer
        )


normalizePos : Array String -> Position -> Position
normalizePos lines (( y, x ) as pos) =
    case Array.get y lines of
        Just line ->
            let
                len =
                    String.length line
            in
                if x >= len then
                    if String.endsWith lineBreak line then
                        ( y + 1, 0 )
                    else
                        ( y, len )
                else
                    pos

        _ ->
            if pos < ( 0, 0 ) then
                ( 0, 0 )
            else
                boundPosition lines


applyDeletion :
    Position
    -> Position
    -> Array String
    -> ( Patch, TextBuffer )
applyDeletion pos1 pos2 buf =
    if pos2 <= pos1 then
        ( Insertion pos1 empty, TextBuffer buf )
    else
        let
            top =
                slice ( 0, 0 ) pos1 buf

            bottom =
                slice pos2 (boundPosition buf) buf

            deleted =
                slice pos1 pos2 buf |> TextBuffer

            res =
                top +++ bottom
        in
            ( Insertion (normalizePos res pos1) deleted
            , TextBuffer res
            )


{-| apply a patch, returns "reversed" patch and result buf
-}
applyPatch : Patch -> TextBuffer -> ( Patch, TextBuffer )
applyPatch patch (TextBuffer buf) =
    case patch of
        Insertion pos s ->
            applyInsertion pos s buf

        Deletion pos1 pos2 ->
            applyDeletion pos1 pos2 buf


(+++) : Array String -> Array String -> Array String
(+++) =
    append


expandTabs : Int -> Int -> String -> List String
expandTabs n firstLineOffset s =
    let
        lines =
            String.split lineBreak s
    in
        List.map2
            (\line start ->
                let
                    tabIndexes =
                        String.indexes "\t" line

                    ( s, lastTabIndex ) =
                        List.foldl
                            (\i ( s, lasti ) ->
                                let
                                    s1 =
                                        s ++ String.slice lasti i line

                                    cnt =
                                        n - (String.length s1 + start) % n

                                    tabs =
                                        String.repeat cnt " "
                                in
                                    ( s1 ++ tabs, i + 1 )
                            )
                            ( "", 0 )
                            tabIndexes
                in
                    s ++ String.dropLeft lastTabIndex line
            )
            lines
            (firstLineOffset :: List.repeat (List.length lines - 1) 0)


lineMaxColumn : String -> Int
lineMaxColumn s =
    let
        len =
            String.length s

        lenLineBreak =
            String.length lineBreak
    in
        if String.right lenLineBreak s == lineBreak then
            len - lenLineBreak
        else
            len


getLineMaxColumn : Int -> TextBuffer -> Int
getLineMaxColumn y lines =
    lines
        |> getLine y
        |> Maybe.map lineMaxColumn
        |> Maybe.withDefault 0


substring : Position -> Position -> TextBuffer -> TextBuffer
substring pos1 pos2 (TextBuffer buf) =
    TextBuffer <| slice pos1 pos2 buf


sliceLines : Int -> Int -> TextBuffer -> TextBuffer
sliceLines begin end (TextBuffer buf) =
    TextBuffer <| slice ( begin, 0 ) ( end, 0 ) buf


sliceRegion : Position -> Position -> TextBuffer -> TextBuffer
sliceRegion begin end (TextBuffer buf) =
    TextBuffer <| slice begin end buf
