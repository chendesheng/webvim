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
        , countLines
        , foldlLines
        , expandTabs
        , mapLines
        , Patch(..)
        , toString
        , getLineMaxColumn
        , mapLinesToList
        )

import Position exposing (..)
import Array exposing (Array)
import String


type TextBuffer
    = TextBuffer (Array String)


type Patch
    = Insertion Position TextBuffer
    | Deletion Position Position


emptyPatch : Patch
emptyPatch =
    Insertion ( 0, 0 ) empty


lineBreak : String
lineBreak =
    "\n"


isEmpty : TextBuffer -> Bool
isEmpty (TextBuffer buf) =
    Array.isEmpty buf


empty : TextBuffer
empty =
    TextBuffer Array.empty


{-| return how many lines of text buffer
-}
countLines : TextBuffer -> Int
countLines (TextBuffer buf) =
    let
        n =
            Array.length buf
    in
        case Array.get (n - 1) buf of
            Nothing ->
                0

            Just line ->
                -- ignore last empty line
                if String.length line == 0 then
                    n - 1
                else
                    n


mapLines : (String -> b) -> TextBuffer -> Array b
mapLines f (TextBuffer buf) =
    Array.map f buf


mapLinesToList : Int -> Int -> (String -> b) -> TextBuffer -> List b
mapLinesToList begin end f (TextBuffer buf) =
    buf
        |> Array.slice begin end
        |> Array.toList
        |> List.map f


foldlLines : Int -> (String -> a -> a) -> a -> TextBuffer -> a
foldlLines n f a (TextBuffer buf) =
    Array.foldl f a <| Array.slice n (Array.length buf) buf


getLine : Int -> TextBuffer -> Maybe String
getLine n (TextBuffer buf) =
    case Array.get n buf of
        Just s ->
            if s == "" then
                Nothing
            else
                Just s

        _ ->
            Nothing


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
    if Array.isEmpty buf then
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
    if String.isEmpty s then
        Array.fromList []
    else
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
                s
                    |> Array.fromList

            lastRow =
                Array.length buf - 1
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
toString (TextBuffer buf) =
    buf
        |> Array.toList
        |> String.join ""


{-| Concat tow textBuffers
-}
append : Array String -> Array String -> Array String
append buf1 buf2 =
    if Array.isEmpty buf1 then
        buf2
    else if Array.isEmpty buf2 then
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
            Array.empty
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
applyInsertion pos s buf =
    let
        ss =
            case s of
                TextBuffer ss ->
                    ss

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
        , top
            +++ ss
            +++ bottom
            |> TextBuffer
        )


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
        in
            ( Insertion pos1 deleted, top +++ bottom |> TextBuffer )


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


getLineMaxColumn : Int -> TextBuffer -> Int
getLineMaxColumn y lines =
    getLine y lines
        |> Maybe.map
            (\s ->
                let
                    len =
                        String.length s
                in
                    if String.right 1 s == lineBreak then
                        len - 1
                    else
                        len
            )
        |> Maybe.withDefault 0
