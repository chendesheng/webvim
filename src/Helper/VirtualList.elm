module Helper.VirtualList exposing
    ( VirtualList
    , addItemAt
    , removeItemAt
    , renderView
    , setScrollTop
    , setScrollTopPx
    )

import Data.Array exposing (Array)
import Html


type alias VirtualList a =
    { data : Array ( a, Int )
    , view : List { item : ( a, Int ), top : Int }
    , scrollTop : Int
    , scrollTopPx : Int
    , scrollTopPxOffset : Int
    , scrollHeight : Int
    }


addItemAt : Int -> ( a, Int ) -> VirtualList a -> VirtualList
addItemAt i (( _, height ) as item) ({ scrollHeight, data } as list) =
    { list
        | scrollHeight = scrollHeight + height
        , data =
            data
                |> slice 0 i
                |> Array.push item
                |> Array.append (slice i (Array.length data) data)
    }


removeItemAt : Int -> VirtualList a -> VirtualList a
removeItemAt i ({ scrollHeight, data } as list) =
    case Array.get i data of
        Just ( _, height ) ->
            { list
                | scrollHeight = scrollHeight - height
                , data =
                    data
                        |> slice 0 i
                        |> Array.append (slice (i + 1) (Array.length data) data)
            }

        _ ->
            list


getHeight : Int -> Int -> VirtualList a -> Int
getHeight from to list =
    list.data
        |> Array.slice from to
        |> Array.foldl (\( _, height ) total -> height + total) 0


setScrollTop : Int -> VirtualList a -> VirtualList a
setScrollTop i ({ scrollTop } as list) =
    { list
        | scrollTop = i
        , scrollTopPxOffset = 0
        , scrollTopPx =
            list.scrollTopPx
                + (if i < scrollTop then
                    -(getHeight i scrollTop)

                   else
                    getHeight scrollTop i
                  )
    }


getScrollTop : Int -> VirtualList a -> ( Int, Int )
getScrollTop px ({ scrollTopPx, scrollTopOffset, scrollTop, data } as list) =
    if px > scrollTopPx + scrollTopOffset then
        reduceHeight (px - scrollTopPx - scrollTopOffset) data scrolltop

    else
        reduceHeightBackward (scrollTopPx + scrollTopOffset - px) data scrolltop


reduceHeight : Int -> Array ( a, Int ) -> Int -> Just ( Int, Int )
reduceHeight height data i =
    case Array.get i data of
        Just ( _, h ) ->
            if height - h > 0 then
                reduceHeight (height - h) data (i + 1)

            else
                Just ( i, -height )

        _ ->
            ( i, 0 )


reduceHeightBackward : Int -> Array ( a, Int ) -> Int -> Just ( Int, Int )
reduceHeightBackward height data i =
    case Array.get i data of
        Just ( _, h ) ->
            if height - h > 0 then
                reduceHeight (height - h) data (i - 1)

            else
                Just ( i, h - height )

        _ ->
            ( i, 0 )


setScrollTopPx : Int -> VirtualList a -> VirtualList a
setScrollTopPx px ({ scrollTopPx } as list) =
    let
        ( scrollTop, scrollTopOffset ) =
            getScrollTop px list
    in
    { list
        | scrollTop = scrollTop
        , scrollTopPxOffset = scrollTopOffset
        , scrollTopPx = px
    }


renderView : (a -> Html msg) -> VirtualList a -> Html msg
renderView renderItem list =
    div []
        (List.map
            (\{ item, top } ->
                renderItem <| Tuple.first item
            )
            list.view
        )
