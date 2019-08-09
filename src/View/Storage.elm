module View.Storage exposing (renderStorage)

import Dict exposing (Dict)
import Fs
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Html.Lazy exposing (..)
import Internal.Window as Win
import Json.Encode as Encode
import Model exposing (registerToString, windowEncoder)
import Model.Buffer exposing (..)
import Model.Frame exposing (Frame)
import Model.Global exposing (..)
import Model.Lint exposing (..)


renderStorage : Global -> Html msg
renderStorage global =
    div [ style "display" "none" ]
        ([ lazy saveRegisters global.registers
         , lazy saveCwd (Fs.workingDir global.fs)
         , div []
            (List.indexedMap
                (\i s ->
                    renderSessionStorageItem
                        ("exHistory[" ++ String.fromInt i ++ "]")
                        s
                )
                global.exHistory
            )
         ]
            ++ (case global.persistent of
                    Just { window, buffers } ->
                        [ lazy saveWindow window
                        , saveBuffers buffers
                        ]

                    _ ->
                        []
               )
        )


saveBuffers : List Buffer -> Html msg
saveBuffers buffers =
    Html.Keyed.node "div"
        []
        (List.indexedMap
            (\i buf ->
                ( buf.id, lazy2 saveBuffer i buf )
            )
            buffers
        )


saveBuffer : Int -> Buffer -> Html msg
saveBuffer i buf =
    buf
        |> bufferToString
        |> renderSessionStorageItem ("buffers[" ++ String.fromInt i ++ "]")


saveWindow : Win.Window Frame -> Html msg
saveWindow win =
    win
        |> windowEncoder
        |> Encode.encode 0
        |> renderSessionStorageItem "window"


renderSessionStorageItem : String -> String -> Html msg
renderSessionStorageItem key value =
    node "session-storage-item"
        [ attribute "key" key
        , attribute "value" value
        ]
        []


saveCwd : String -> Html msg
saveCwd cwd =
    renderSessionStorageItem "cwd" cwd


saveRegisters : Dict String RegisterText -> Html msg
saveRegisters registers =
    registers
        |> registerToString
        |> renderSessionStorageItem "registers"
