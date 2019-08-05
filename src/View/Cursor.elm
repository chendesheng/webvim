module View.Cursor exposing (renderCursor, renderCursorInner, renderMatchedCursor)

import Font exposing (FontInfo)
import Helper.Helper exposing (px, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Ime exposing (IME, emptyIme, isImeActive, isImeComposing, renderIme)
import Internal.Position exposing (Position)
import Internal.TextBuffer as B
import Model.Buffer exposing (..)
import Update.Cursor exposing (cursorPoint)
import Update.Message exposing (Msg(..))


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
            isMainCursor && isImeActive ime

        imeIsComposing =
            imeIsActive && isImeComposing ime
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
        [ renderIme fontInfo ime |> Html.map IMEMessage ]


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
