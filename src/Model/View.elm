module Model.View exposing
    ( View
    , emptyView
    , resizeView
    , scrollViewLines
    , setCursor
    , setCursorColumn
    , setScrollTop
    , viewDecoder
    , viewEncoder
    )

import Helper.Helper
    exposing
        ( extname
        , filename
        , findFirst
        , rangeCount
        , regex
        , relativePath
        )
import Internal.Position exposing (Position, cursorDecoder, cursorEncoder)
import Json.Decode as Decode
import Json.Encode as Encode
import Model.Size exposing (Size)


type alias View =
    { bufId : String
    , cursor : Position
    , cursorColumn : Int
    , scrollTop : Int
    , scrollTopPx : Int
    , scrollLeftPx : Int
    , matchedCursor : Maybe ( Position, Position )
    , lines : List Int
    , gutterLines : List Int
    , size : Size
    }


emptyView : View
emptyView =
    { bufId = ""
    , cursor = ( 0, 0 )
    , cursorColumn = 0
    , scrollTop = 0
    , scrollTopPx = 0
    , scrollLeftPx = 0
    , matchedCursor = Nothing
    , lines = rangeCount 0 3
    , gutterLines = rangeCount 0 3
    , size = { width = 1, height = 1 }
    }


viewEncoder : View -> Encode.Value
viewEncoder view =
    Encode.object
        [ ( "bufId", Encode.string view.bufId )
        , ( "cursor", cursorEncoder view.cursor )
        , ( "scrollTop", Encode.int view.scrollTop )
        ]


viewDecoder : Int -> Decode.Decoder View
viewDecoder lineHeight =
    Decode.map3
        (\bufId cursor scrollTop ->
            { emptyView
                | bufId = bufId
                , cursor = cursor
                , cursorColumn = Tuple.second cursor
                , scrollTop = scrollTop
                , scrollTopPx = scrollTop * lineHeight
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


setScrollTop : Int -> Int -> View -> View
setScrollTop n lineHeight view =
    if n == view.scrollTop then
        view

    else
        let
            viewLines =
                scrollViewLines view.size.height view.scrollTop n view.lines
        in
        { view
            | scrollTop = n
            , scrollTopPx =
                if n == view.scrollTopPx // lineHeight then
                    view.scrollTopPx

                else
                    n * lineHeight
            , lines = viewLines
            , gutterLines =
                if view.scrollTop == n then
                    view.gutterLines

                else
                    viewLines
        }
