module Model.View exposing
    ( View
    , emptyView
    , resizeView
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
