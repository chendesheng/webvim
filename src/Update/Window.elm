module Update.Window exposing (..)

import Internal.Window as Win exposing (Window)
import Model exposing (View, emptyView)


nextView : Window View -> Window View
nextView win =
    win


changeViewBuffer : Int -> Window View -> Window View
changeViewBuffer bufId =
    Win.updateView (\view -> { emptyView | bufId = bufId })


getActiveView : Window View -> View
getActiveView win =
    win
        |> Win.getActiveView
        |> Maybe.withDefault emptyView
