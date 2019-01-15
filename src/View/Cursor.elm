module View.Cursor exposing (renderCursor, renderCursorInner, renderMatchedCursor)

import Helper.Helper exposing (px, rem)
import Helper.KeyEvent exposing (decodeKeyboardEvent)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Lazy exposing (..)
import Internal.Position exposing (Position)
import Internal.TextBuffer as B
import Json.Decode as Decode
import Json.Encode as Encode
import Model exposing (FontInfo, Global, IME, LintError, Mode(..), emptyIme)
import Update.Cursor exposing (cursorPoint)
import Update.Message exposing (IMEMsg(..), Msg(..))


renderCursor :
    Bool
    -> FontInfo
    -> IME
    -> B.TextBuffer
    -> String
    -> Maybe Position
    -> Html Msg
renderCursor isActive fontInfo ime lines classname cursor =
    case cursor of
        Just ( y, x ) ->
            lazy8 renderCursorInner isActive True fontInfo ime lines classname y x

        _ ->
            text ""


renderCursorInner :
    Bool
    -> Bool
    -> FontInfo
    -> IME
    -> B.TextBuffer
    -> String
    -> Int
    -> Int
    -> Html Msg
renderCursorInner isActive isMainCursor fontInfo ime lines classname y x =
    let
        ( ( by, bx ), ( ey, ex ) ) =
            cursorPoint fontInfo lines y x

        imeIsActive =
            isMainCursor && ime.isActive

        imeIsComposing =
            imeIsActive && ime.isComposing
    in
    div
        ([ class "cursor"
         , class classname
         , classList [ ( "cursor-not-active", not isActive ) ]
         , style "left" <| px bx
         , style "top" <| rem y
         , style "width" <| px (ex - bx)
         ]
            ++ (if imeIsActive then
                    [ class "cursor-ime-active" ]

                else
                    []
               )
            ++ (if imeIsComposing then
                    [ class "cursor-ime-composing" ]

                else
                    []
               )
        )
        [ if imeIsActive then
            let
                renderInput =
                    if ime.isSafari then
                        renderInputSafari

                    else
                        renderInputChrome
            in
            lazy3 renderInput fontInfo imeIsComposing ime.compositionText

          else
            noCompositionInput
        ]


renderInputSafari : FontInfo -> Bool -> String -> Html Msg
renderInputSafari fontInfo isComposing _ =
    span
        ((if isComposing then
            []

          else
            [ style "opacity" "0" ]
         )
            ++ [ id "hidden-input"
               , contenteditable True
               , Events.custom "compositionstart"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionStart text
                                , stopPropagation = False
                                , preventDefault = False
                                }
                             --|> Debug.log "compositionstart"
                            )
                    )

               -- in safari keydown event is after `compositionstart` event
               , Events.custom "keydown"
                    (decodeKeyboardEvent False
                        |> Decode.map
                            (\key ->
                                { message = IMEMessage <| CompositionTry key
                                , stopPropagation = True
                                , preventDefault = True
                                }
                             --|> Debug.log "keydown"
                            )
                    )
               , Events.custom "compositionend"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionCommit text
                                , stopPropagation = True
                                , preventDefault = True
                                }
                             --|> Debug.log "compositionend"
                            )
                    )
               , Events.custom "textInput" <|
                    Decode.succeed
                        { message = NoneMessage
                        , stopPropagation = True
                        , preventDefault = True
                        }
               ]
        )
        []


renderInputChrome : FontInfo -> Bool -> String -> Html Msg
renderInputChrome fontInfo isComposing compositionText =
    span
        ((if isComposing then
            []

          else
            [ Events.custom "keydown"
                (decodeKeyboardEvent False
                    |> Decode.map
                        (\key ->
                            { message = IMEMessage (CompositionWait key)
                            , stopPropagation = True
                            , preventDefault = True
                            }
                         --|> Debug.log "keydown"
                        )
                )
            , style "opacity" "0"
            , property "textContent" (Encode.string "")
            ]
         )
            ++ [ id "hidden-input"
               , contenteditable True
               , Events.custom "compositionstart"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionStart text
                                , stopPropagation = False
                                , preventDefault = False
                                }
                            )
                     --|> Decode.map (Debug.log "compositionstart")
                    )
               , Events.custom "compositionupdate"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message = IMEMessage <| CompositionStart text
                                , stopPropagation = False
                                , preventDefault = False
                                }
                            )
                     --|> Decode.map (Debug.log "compositionstart")
                    )
               , Events.custom "compositionend"
                    (Decode.field "data" Decode.string
                        |> Decode.map
                            (\text ->
                                { message =
                                    IMEMessage <|
                                        CompositionCommit <|
                                            --Debug.log "compositionend.data" <|
                                            text
                                , stopPropagation = True
                                , preventDefault = True
                                }
                            )
                    )
               ]
        )
        []


noCompositionInput : Html Msg
noCompositionInput =
    span
        [ id "hidden-input"
        , tabindex 0
        , autofocus True
        , onKeyDownPressKeys True
        , style "opacity" "0"
        ]
        []


onKeyDownPressKeys replaceFullwithToHalfWidth =
    Events.custom "keydown"
        (decodeKeyboardEvent replaceFullwithToHalfWidth
            |> Decode.map
                (\key ->
                    { message = PressKeys key
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
        )


renderMatchedCursor :
    Bool
    -> FontInfo
    -> B.TextBuffer
    -> Mode
    -> Position
    -> Maybe ( Position, Position )
    -> List (Html Msg)
renderMatchedCursor isActive fontInfo lines mode cursor matchedCursor =
    case mode of
        Ex _ ->
            []

        _ ->
            case matchedCursor of
                Just ( a, b ) ->
                    [ a, b ]
                        |> List.filter ((/=) cursor)
                        |> List.map
                            (\( y, x ) ->
                                renderCursorInner isActive
                                    False
                                    fontInfo
                                    emptyIme
                                    lines
                                    "matched-cursor"
                                    y
                                    x
                            )

                _ ->
                    []
