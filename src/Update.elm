module Update exposing (update)

import Model exposing (..)
import Message exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    ( model, Cmd.none )
