module Menu exposing
    ( Model
    , getSelected
    , init
    , render
    , selectBackward
    , selectForward
    , show
    )

import Helper.Helper exposing (repeatfn)
import Html exposing (..)
import Html.Attributes exposing (..)
import Zipper exposing (Zipper)



-- MODEL


type Model a
    = Model (InnerModel a)


type alias InnerModel a =
    { scrollPos : Zipper a -- zipper.current is the top item in view
    , selected : Maybe Int
    , maxItems : Int
    }


init : Int -> List a -> Model a
init maxItems list =
    Model
        { scrollPos = Zipper.fromList list
        , selected = Nothing
        , maxItems = Basics.min maxItems (List.length list)
        }


getSelected : Model a -> Maybe a
getSelected (Model { scrollPos, selected }) =
    Maybe.andThen (\i -> Zipper.getNth i scrollPos) selected



-- UPDATE


{-| Select next item, scroll down if hit the boundary,
select nothing if hit bottom
-}
selectForward : Model a -> Model a
selectForward (Model m) =
    case m.selected of
        Just i ->
            if i + 1 < m.maxItems then
                Model { m | selected = Just (i + 1) }

            else
                m
                    |> scrollForward
                    |> Maybe.withDefault { m | selected = Nothing }
                    |> Model

        _ ->
            Model
                { m
                    | scrollPos = Zipper.rewind m.scrollPos
                    , selected = Just 0
                }


{-| Select next item, scroll up if hit the boundary,
select nothing if hit top
-}
selectBackward : Model a -> Model a
selectBackward (Model m) =
    case m.selected of
        Just i ->
            if i - 1 >= 0 then
                Model { m | selected = Just (i - 1) }

            else
                m
                    |> scrollBackward
                    |> Maybe.withDefault { m | selected = Nothing }
                    |> Model

        _ ->
            Model <| scrollToBottom m


scrollToBottom : InnerModel a -> InnerModel a
scrollToBottom m =
    case scrollForward m of
        Just m1 ->
            scrollToBottom m1

        Nothing ->
            { m | selected = Just <| m.maxItems - 1 }


scrollForward : InnerModel a -> Maybe (InnerModel a)
scrollForward m =
    repeatfn m.maxItems Zipper.moveForward m.scrollPos
        |> Maybe.andThen
            (\_ ->
                Zipper.moveForward m.scrollPos
                    |> Maybe.map (\z -> { m | scrollPos = z })
            )


scrollBackward : InnerModel a -> Maybe (InnerModel a)
scrollBackward m =
    m.scrollPos
        |> Zipper.moveBackward
        |> Maybe.map (\z -> { m | scrollPos = z })



-- VIEW


show : Model a -> List a
show (Model { scrollPos, maxItems, selected }) =
    scrollPos
        |> Zipper.getForwards
        |> List.take maxItems


render : (Bool -> a -> Html msg) -> Model a -> List (Html msg)
render renderItem (Model { scrollPos, maxItems, selected }) =
    scrollPos
        |> Zipper.getForwards
        |> List.take maxItems
        |> List.indexedMap
            (\i ->
                renderItem
                    (Maybe.map ((==) i) selected
                        |> Maybe.withDefault False
                    )
            )
