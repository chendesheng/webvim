module Ime exposing
    ( IME
    , IMEMsg
    , emptyIme
    , focusIme
    , isImeActive
    , isImeComposing
    , renderIme
    , setImeActive
    , update
    )

import Browser.Dom as Dom
import Helper.Helper exposing (px)
import Helper.KeyEvent exposing (decodeKeyboardEvent)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Lazy exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Task



{- two cases:
   ## System IME disabled
       - keydown: save to Composing.str
       - delay 3ms then, stop if not start composing

   ## System IME enabled
       - keydown: save to Composing.str
       - compositionstart: overwrite Composing.str
       - compositionupdate: update Composing.str
       - compositionend: call onKeyPress
-}
-- model


type IME
    = NotActive
    | Active String
    | Composing String
      -- for clear text state inside control
    | CompositionClear


isImeActive ime =
    ime /= NotActive


isImeComposing ime =
    case ime of
        Composing _ ->
            True

        _ ->
            False


focusIme : Cmd IMEMsg
focusIme =
    Task.attempt
        (always NoOp)
        (Dom.focus "hidden-input")


emptyIme : IME
emptyIme =
    NotActive


setImeActive : Bool -> IME -> IME
setImeActive isActive ime =
    let
        _ =
            Debug.log "setImeActive" isActive
    in
    if isActive then
        case ime of
            CompositionClear ->
                Active ""

            NotActive ->
                Active ""

            _ ->
                ime

    else
        NotActive



-- update


type IMEMsg
    = CompositionStart String
    | CompositionUpdate String
    | CompositionEnd String
      -- It's like CompositionEnd but without text
      -- only Active ime state will handle CompositionStop
    | CompositionStop
      -- when composition end/stop, we set ime to NotActive first,
      -- then, after 3ms, send an ActiveIme message to set ime active again
    | ActiveIme
    | NoOp


update : (IMEMsg -> msg) -> (String -> Cmd msg) -> IMEMsg -> IME -> ( IME, Cmd msg )
update toMsg onKeyPress imeMsg ime =
    let
        _ =
            Debug.log "update" ime
    in
    case ime of
        NotActive ->
            case imeMsg of
                CompositionEnd s ->
                    ( NotActive
                    , onKeyPress s
                    )

                _ ->
                    ( ime, Cmd.none )

        Active str ->
            case imeMsg of
                CompositionStart s ->
                    let
                        _ =
                            Debug.log "CompositionStart" s
                    in
                    ( Composing s, Cmd.none )

                CompositionUpdate s ->
                    ( Active s
                      -- delay a little bit
                      -- then stop if still not in Composing state
                    , Process.sleep 3
                        |> Task.perform (always <| toMsg CompositionStop)
                    )

                CompositionStop ->
                    stopComposition toMsg onKeyPress ime str

                _ ->
                    ( ime, Cmd.none )

        Composing str ->
            case imeMsg of
                CompositionUpdate s ->
                    ( if str == s then
                        ime

                      else
                        Composing s
                    , Cmd.none
                    )

                CompositionEnd s ->
                    stopComposition toMsg onKeyPress ime s

                _ ->
                    ( ime, Cmd.none )

        CompositionClear ->
            case imeMsg of
                ActiveIme ->
                    ( setImeActive True ime, Cmd.map toMsg focusIme )

                _ ->
                    ( ime, Cmd.none )


stopComposition toMsg onKeyPress ime s =
    ( CompositionClear
    , Cmd.batch
        [ onKeyPress s
        , Cmd.map toMsg sendActiveIme
        ]
    )


sendActiveIme : Cmd IMEMsg
sendActiveIme =
    Process.sleep 50
        |> Task.perform (always ActiveIme)



-- render


renderIme : IME -> Html IMEMsg
renderIme ime =
    let
        _ =
            Debug.log "ime" ime
    in
    lazy renderInput ime


renderInput : IME -> Html IMEMsg
renderInput ime =
    hiddenInput
        (case ime of
            NotActive ->
                [ property "textContent" (Encode.string "")
                , contenteditable False
                , style "opacity" "0"
                , Events.custom "keydown"
                    (decodeKeyboardEvent True
                        |> Decode.map
                            (\key ->
                                { message = CompositionEnd key
                                , stopPropagation = True
                                , preventDefault = True
                                }
                            )
                    )
                ]

            Active _ ->
                [ contenteditable True
                , style "opacity" "0"
                , Events.custom "keydown"
                    (decodeKeyboardEvent False
                        |> Decode.map
                            (\key ->
                                { message = CompositionUpdate key
                                , stopPropagation = True
                                , preventDefault = True
                                }
                            )
                    )
                , Events.on "compositionstart"
                    (Decode.map CompositionStart
                        (Decode.field "data" Decode.string)
                    )
                ]

            Composing _ ->
                [ contenteditable True
                , Events.on "compositionupdate"
                    (Decode.map CompositionUpdate
                        (Decode.field "data" Decode.string)
                    )
                , Events.on "compositionend"
                    (Decode.field "data" Decode.string
                        |> Decode.map CompositionEnd
                    )
                , Events.on "compositionstart"
                    (Decode.map CompositionStart
                        (Decode.field "data" Decode.string)
                    )
                ]

            CompositionClear ->
                [ contenteditable False
                , property "textContent" (Encode.string "")
                ]
        )


hiddenInput : List (Attribute msg) -> Html msg
hiddenInput props =
    -- we can use a span or an input here, span is much easier to align
    span
        ([ id "hidden-input"
         , tabindex 0
         , autofocus True

         --turn off auto fixups
         , autocomplete False
         , spellcheck False
         , property "autocorrect" (Encode.string "off")
         , property "autocapitalize" (Encode.string "off")
         ]
            ++ props
        )
        []
