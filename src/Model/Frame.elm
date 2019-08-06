module Model.Frame exposing
    ( Frame
    , addOrActiveView
    , empty
    , frameDecoder
    , frameEncoder
    , getActiveView
    , getActiveViewId
    , getAlterViewId
    , getJumps
    , getView
    , getViews
    , newView
    , resize
    , updateActiveView
    , updateJumps
    , updateView
    )

import Helper.Helper as Helper
import Internal.Jumps exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Model.Size exposing (Size, emptySize)
import Model.View exposing (..)
import Zipper


type Frame
    = Frame FrameStruct


type alias FrameStruct =
    { views : List View
    , jumps : Jumps
    , size : Size
    }


empty : Frame
empty =
    Frame emptyFrameStruct


emptyFrameStruct : FrameStruct
emptyFrameStruct =
    { views = []
    , jumps = Zipper.empty
    , size = emptySize
    }


frameEncoder : Frame -> Encode.Value
frameEncoder (Frame { views }) =
    Encode.object [ ( "views", Encode.list viewEncoder views ) ]


frameDecoder : Decode.Decoder Frame
frameDecoder =
    Decode.map (\views -> Frame { emptyFrameStruct | views = views })
        (Decode.field "views" <| Decode.list <| viewDecoder)


getActiveView : Frame -> Maybe View
getActiveView (Frame { views, size }) =
    List.head views
        |> Maybe.map (\view -> { view | size = size, isActive = True })


getActiveViewId : Frame -> Maybe String
getActiveViewId (Frame frame) =
    getActiveViewIdHelper frame


getActiveViewIdHelper : FrameStruct -> Maybe String
getActiveViewIdHelper { views } =
    views |> List.head |> Maybe.map .bufId


getAlterViewId : Frame -> Maybe String
getAlterViewId (Frame { views }) =
    case views of
        _ :: b :: _ ->
            Just b.bufId

        _ ->
            Nothing


getView : String -> Frame -> Maybe View
getView id ((Frame { views }) as frame) =
    let
        isActive =
            Just id == getActiveViewId frame
    in
    Helper.findFirst (\{ bufId } -> id == bufId) views
        |> Maybe.map (\view -> { view | isActive = isActive })


addOrActiveView : View -> Frame -> Frame
addOrActiveView view (Frame frame) =
    Frame
        { frame
            | views =
                resizeView frame.size view
                    :: List.filter (\{ bufId } -> bufId /= view.bufId) frame.views
        }


updateView : String -> (View -> View) -> Frame -> Frame
updateView id fn (Frame frame) =
    let
        isActive =
            Just id == getActiveViewIdHelper frame
    in
    Frame
        { frame
            | views =
                List.map
                    (\({ bufId } as v) ->
                        if bufId == id then
                            fn { v | isActive = isActive }

                        else
                            v
                    )
                    frame.views
        }


updateActiveView : (View -> View) -> Frame -> Frame
updateActiveView fn (Frame frame) =
    Frame
        (case frame.views of
            v :: rest ->
                { frame | views = fn { v | isActive = True } :: rest }

            _ ->
                frame
        )


getViews : Frame -> List View
getViews (Frame frame) =
    frame.views


getJumps : Frame -> Jumps
getJumps (Frame { jumps }) =
    jumps


updateJumps : (Jumps -> Jumps) -> Frame -> Frame
updateJumps fn (Frame frame) =
    Frame { frame | jumps = fn frame.jumps }


resize : Size -> Frame -> Frame
resize size (Frame frame) =
    Frame
        { frame
            | size = size
            , views =
                -- only active view need resize
                case frame.views of
                    activeView :: rest ->
                        (activeView
                            |> resizeView size
                            |> scrollToCursor
                        )
                            :: rest

                    _ ->
                        frame.views
        }


newView : String -> Frame -> View
newView bufId (Frame frame) =
    resizeView frame.size { emptyView | bufId = bufId }
