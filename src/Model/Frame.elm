module Model.Frame exposing
    ( Frame
    , addOrActiveView
    , emptyFrame
    , frameDecoder
    , frameEncoder
    , getActiveView
    , getActiveViewId
    , getAlterViewId
    , getView
    , updateActiveView
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


type alias Frame =
    { views : List View
    , jumps : Jumps
    , size : Size
    }


emptyFrame : Frame
emptyFrame =
    { views = []
    , jumps = Zipper.empty
    , size = emptySize
    }


frameEncoder : Frame -> Encode.Value
frameEncoder { views } =
    Encode.object [ ( "views", Encode.list viewEncoder views ) ]


frameDecoder : Int -> Decode.Decoder Frame
frameDecoder lineHeight =
    Decode.map (\views -> { emptyFrame | views = views })
        (Decode.field "views" <| Decode.list <| viewDecoder lineHeight)


getActiveView : Frame -> Maybe View
getActiveView { views } =
    List.head views


getActiveViewId : Frame -> Maybe String
getActiveViewId { views } =
    views |> List.head |> Maybe.map .bufId


getAlterViewId : Frame -> Maybe String
getAlterViewId { views } =
    case views of
        a :: b :: rest ->
            Just b.bufId

        _ ->
            Nothing


getView : String -> Frame -> Maybe View
getView id { views } =
    Helper.findFirst (\{ bufId } -> id == bufId) views


addOrActiveView : View -> Frame -> Frame
addOrActiveView view frame =
    { frame
        | views =
            resizeView frame.size view
                :: List.filter
                    (\{ bufId } ->
                        bufId /= view.bufId
                    )
                    frame.views
    }


updateView : String -> (View -> View) -> Frame -> Frame
updateView id fn frame =
    { frame
        | views =
            List.map
                (\({ bufId } as v) ->
                    if bufId == id then
                        fn v

                    else
                        v
                )
                frame.views
    }


updateActiveView : (View -> View) -> Frame -> Frame
updateActiveView fn frame =
    { frame
        | views =
            case frame.views of
                v :: rest ->
                    fn v :: rest

                _ ->
                    frame.views
    }
