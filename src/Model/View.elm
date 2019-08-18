module Model.View exposing
    ( View
    , applyDiffToView
    , applyRegionChangeToView
    , correctCursor
    , correctPosition
    , emptyView
    , resizeView
    , scrollToCursor
    , scrollViewLines
    , setCursor
    , setCursorColumn
    , setScrollTop
    , viewDecoder
    , viewEncoder
    )

import Helper.Helper exposing (rangeCount)
import Internal.Position exposing (Position, cursorDecoder, cursorEncoder)
import Internal.Syntax exposing (Syntax, emptySyntax)
import Internal.TextBuffer as B exposing (RegionChange(..), TextBuffer)
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Size exposing (Size)


type alias View =
    { bufId : String
    , cursor : Position
    , cursorColumn : Int
    , scrollTop : Int
    , scrollTopOffsetPx : Int
    , scrollLeftPx : Int
    , matchedCursor : Maybe ( Position, Position )
    , lines : List Int
    , gutterLines : List Int
    , size : Size
    , isActive : Bool
    , syntax : Syntax
    }


emptyView : View
emptyView =
    { bufId = ""
    , cursor = ( 0, 0 )
    , cursorColumn = 0
    , scrollTop = 0
    , scrollTopOffsetPx = 0
    , scrollLeftPx = 0
    , matchedCursor = Nothing
    , lines = rangeCount 0 3
    , gutterLines = rangeCount 0 3
    , size = { width = 1, height = 1 }
    , isActive = False
    , syntax = emptySyntax
    }


viewEncoder : View -> Encode.Value
viewEncoder view =
    Encode.object
        [ ( "bufId", Encode.string view.bufId )
        , ( "cursor", cursorEncoder view.cursor )
        , ( "scrollTop", Encode.int view.scrollTop )
        ]


viewDecoder : Decode.Decoder View
viewDecoder =
    Decode.map3
        (\bufId cursor scrollTop ->
            { emptyView
                | bufId = bufId
                , cursor = cursor
                , cursorColumn = Tuple.second cursor
                , scrollTop = scrollTop
                , scrollTopOffsetPx = 0
            }
        )
        (Decode.field "bufId" Decode.string)
        (Decode.field "cursor" cursorDecoder)
        (Decode.field "scrollTop" Decode.int)


resizeView : Size -> View -> View
resizeView size view =
    if size == view.size then
        view

    else if size.height == view.size.height then
        { view | size = size }

    else
        let
            viewLines =
                rangeCount view.scrollTop <| size.height + 2
        in
        { view
            | size = size
            , lines = viewLines
            , gutterLines = viewLines
        }


setCursor : Position -> Bool -> View -> View
setCursor cursor saveColumn view =
    { view
        | cursor = cursor
        , cursorColumn =
            if saveColumn then
                Tuple.second cursor

            else
                view.cursorColumn
    }


setCursorColumn : Int -> View -> View
setCursorColumn cursorColumn view =
    { view | cursorColumn = cursorColumn }


scrollViewLinesHelper : Int -> Int -> Int -> Int -> List Int -> List Int
scrollViewLinesHelper height from to i viewLines =
    -- height MUST equal to List.length viewLines
    -- from <= n < from + height
    case viewLines of
        n :: rest ->
            if to <= n && n < to + height then
                n :: scrollViewLinesHelper height from to i rest

            else
                -- n < to || to + height <= n
                i :: scrollViewLinesHelper height from to (i + 1) rest

        _ ->
            []


scrollViewLines : Int -> Int -> Int -> List Int -> List Int
scrollViewLines height_ from to viewLines =
    let
        height =
            height_ + 2
    in
    if from == to then
        viewLines

    else if from + height <= to || to + height <= from then
        rangeCount to height

    else if from > to then
        -- show up contents
        scrollViewLinesHelper height from to to viewLines

    else if from < to then
        -- show down contents
        -- Prove of i < to + height:
        --   n is item in viewLines, this means from <= n < from + height
        --   also when to <= n < to + height, n will preserve
        --   thus when to <= n < from + height, n will preserve
        --   thus (from + height - to) items in viewLines will preserve
        --   thus i will increase height - (from + height - to) times
        --   since i start from (from + height),
        --     i < (from + height) + height - (from + height - to) = to + height
        scrollViewLinesHelper height from to (from + height) viewLines

    else
        viewLines


setScrollTop : Int -> View -> View
setScrollTop n view =
    if n == view.scrollTop then
        view

    else
        let
            viewLines =
                scrollViewLines view.size.height view.scrollTop n view.lines
        in
        { view
            | scrollTop = n
            , scrollTopOffsetPx = 0
            , lines = viewLines
            , gutterLines =
                if view.scrollTop == n then
                    view.gutterLines

                else
                    viewLines
        }


applyInsertionToView : Int -> Int -> Int -> Int -> List Int -> List Int
applyInsertionToView from size scrollTop scrollBottom viewLines =
    if size == 0 || from >= scrollBottom then
        viewLines

    else
        applyInsertionToViewHelper from
            size
            scrollBottom
            (max from scrollTop)
            viewLines


{-| start at [ 0 , 1 , 2 ]

Ins 0 1
[ 1 , 2 , 0 ]

Ins 1 2
[ 1 , 2 , 0 ]

Ins 0 1
[ 2 , 0 , 1 ]

Ins 2 1
[ 2 , 0 , 1 ]

-}
applyInsertionToViewHelper : Int -> Int -> Int -> Int -> List Int -> List Int
applyInsertionToViewHelper from size scrollBottom i viewLines =
    -- go through each line
    -- if n < from, keep it not change
    -- if n >= from, increase size
    -- after increase size check if it's >= scrollBottom
    -- if it is, replace it to inserting line
    case viewLines of
        n :: rest ->
            if n < from then
                n :: applyInsertionToViewHelper from size scrollBottom i rest

            else if n + size < scrollBottom then
                (n + size) :: applyInsertionToViewHelper from size scrollBottom i rest

            else
                i :: applyInsertionToViewHelper from size scrollBottom (i + 1) rest

        _ ->
            []


applyDeletionToView : Int -> Int -> Int -> List Int -> List Int
applyDeletionToView from to scrollBottom viewLines =
    let
        size =
            to - from
    in
    if size <= 0 || from >= scrollBottom then
        viewLines

    else
        applyDeletionToViewHelper from
            to
            size
            (max from (scrollBottom - size))
            viewLines


applyDeletionToViewHelper : Int -> Int -> Int -> Int -> List Int -> List Int
applyDeletionToViewHelper from to size i viewLines =
    case viewLines of
        n :: rest ->
            if n < from then
                n :: applyDeletionToViewHelper from to size i rest

            else if n < to then
                i :: applyDeletionToViewHelper from to size (i + 1) rest

            else
                (n - size) :: applyDeletionToViewHelper from to size i rest

        _ ->
            []


applyDiffToView : List RegionChange -> View -> View
applyDiffToView diff ({ scrollTop, size, lines } as view) =
    { view
        | lines =
            List.foldl
                (\change lines1 ->
                    applyRegionChangeToView change scrollTop (size.height + 2) lines1
                )
                lines
                diff
    }


applyRegionChangeToView : RegionChange -> Int -> Int -> List Int -> List Int
applyRegionChangeToView change scrollTop height viewLines =
    let
        scrollBottom =
            scrollTop + height
    in
    case change of
        RegionAdd ( ( by, _ ), ( ey, _ ) ) ->
            applyInsertionToView by (ey - by) scrollTop scrollBottom viewLines

        RegionRemove ( ( by, _ ), ( ey, _ ) ) ->
            applyDeletionToView by ey scrollBottom viewLines


{-| scroll to ensure pos it is insdie viewport
-}
scrollTo : Int -> View -> View
scrollTo y view =
    let
        miny =
            view.scrollTop

        maxy =
            miny + view.size.height - 1

        scrollTop =
            if miny > y then
                y

            else if y > maxy then
                y - maxy + miny

            else
                miny

        updateOffsetPx v =
            if v.scrollTop == y then
                { v | scrollTopOffsetPx = 0 }

            else
                v
    in
    view
        |> setScrollTop scrollTop
        |> updateOffsetPx


scrollToCursor : View -> View
scrollToCursor view =
    scrollTo (Tuple.first view.cursor) view


correctCursor : Bool -> TextBuffer -> View -> View
correctCursor excludeLinkBreak lines view =
    setCursor
        (correctPosition view.cursor excludeLinkBreak lines)
        False
        view


correctPosition : Position -> Bool -> TextBuffer -> Position
correctPosition pos excludeLineBreak lines =
    let
        ( y, x ) =
            pos

        y1 =
            0
                |> max (B.count lines - 2)
                |> min y

        maxcol =
            B.getLineMaxColumn y1 lines
                - (if excludeLineBreak then
                    String.length B.lineBreak

                   else
                    0
                  )

        x1 =
            maxcol
                |> max 0
                |> min x
    in
    if y1 == y && x == x1 then
        correctPositionOnSurrogate lines pos

    else
        correctPositionOnSurrogate lines ( y1, x1 )


correctPositionOnSurrogate : B.TextBuffer -> Position -> Position
correctPositionOnSurrogate lines (( y, x ) as pos) =
    lines
        |> B.getLine y
        |> Maybe.andThen
            (\line ->
                line
                    |> String.dropLeft (x - 1)
                    |> String.uncons
                    |> Maybe.map
                        (\( ch, _ ) ->
                            if (ch |> String.fromChar |> String.length) > 1 then
                                if x + 1 >= String.length line - 1 then
                                    ( y, x - 1 )

                                else
                                    ( y, x + 1 )

                            else
                                pos
                        )
            )
        |> Maybe.withDefault pos
