module View.StatusBar exposing (renderStatusBar)

import Font exposing (FontInfo)
import Helper.Helper exposing (ch, rem)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Ime exposing (IME)
import Internal.TextBuffer as B
import Model exposing (Global, LintError, Mode)
import Update.Buffer as Buf
import Update.Message exposing (Msg(..))
import View.Cursor exposing (renderCursorInner)
import Vim.Helper exposing (parseKeys)


renderStatusBar :
    FontInfo
    -> IME
    -> Bool
    -> Mode
    -> String
    -> List LintError
    -> String
    -> Html Msg
renderStatusBar fontInfo ime dirty mode continuation items name =
    div
        [ class "status"
        , classList [ ( "dirty", dirty ) ]
        ]
        [ lazy3 renderStatusBarLeft fontInfo ime mode
        , lazy3 renderStatusBarRight continuation name items
        ]


renderStatusBarLeft : FontInfo -> IME -> Mode -> Html Msg
renderStatusBarLeft fontInfo ime mode =
    let
        statusBar =
            Buf.getStatusBar mode

        divs =
            case statusBar.cursor of
                Just ( y, x ) ->
                    [ renderCursorInner True
                        True
                        fontInfo
                        ime
                        (B.fromString statusBar.text)
                        "ex-cursor"
                        0
                        x
                    ]

                _ ->
                    []
    in
    div [ class "status-left" ]
        (div
            [ class
                (if statusBar.error then
                    "status-error"

                 else
                    "status-info"
                )
            ]
            [ text
                (if statusBar.text == "-- Normal --" then
                    ""

                 else
                    statusBar.text
                )
            ]
            :: divs
        )


renderStatusBarRight : String -> String -> List LintError -> Html msg
renderStatusBarRight continuation name items =
    div [ class "status-right" ]
        [ div
            [ class "status-cmds" ]
            [ continuation
                |> parseKeys
                |> String.join ""
                |> text
            ]
        , div [ class "filename" ] [ text name ]
        , lazy renderLintStatus items
        ]


renderLintStatus : List LintError -> Html msg
renderLintStatus items =
    let
        errors =
            items
                |> List.filter (.tipe >> (/=) "warning")
                |> List.length

        warnings =
            items
                |> List.filter (.tipe >> (==) "warning")
                |> List.length
    in
    div
        [ class "lint-status" ]
        [ span
            [ classList
                [ ( "highlight-errors-icon", errors > 0 ) ]
            ]
            [ i
                [ class "fas fa-times-circle" ]
                []
            , errors
                |> String.fromInt
                |> text
            ]
        , span
            [ classList [ ( "highlight-warnings-icon", warnings > 0 ) ] ]
            [ i
                [ class "fas fa-exclamation-triangle" ]
                []
            , warnings
                |> String.fromInt
                |> text
            ]
        ]
