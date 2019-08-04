module Update.Mouse exposing (onMouseWheel, onResize)

import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Window as Win
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Frame as Frame exposing (Frame, emptyFrame)
import Model.Global exposing (..)
import Model.Size exposing (..)
import Model.View as View exposing (..)
import Update.Message exposing (..)
import Update.Vim exposing (..)
import Vim.AST as V exposing (AST, Operator(..), isEditingOperator)


onMouseWheel : Win.Path -> Int -> Int -> Editor -> ( Editor, Cmd Msg )
onMouseWheel path deltaY deltaX ({ buf, global } as ed) =
    let
        view =
            buf.view

        lineHeight =
            global.lineHeight

        scrollTopPx =
            (view.scrollTopPx + deltaY)
                |> max 0
                |> min ((B.count buf.lines - 2) * lineHeight)

        scrollTopDelta =
            scrollTopPx // lineHeight - buf.view.scrollTop
    in
    { ed
        | buf =
            { buf
                | view =
                    { view
                        | scrollTopPx = scrollTopPx

                        --, scrollLeftPx =
                        --    (view.scrollLeftPx + deltaX)
                        --        |> max 0
                    }
            }
    }
        |> applyVimAST False
            "<mousewheel>"
            { count = Nothing
            , edit = Just <| V.Scroll <| V.ScrollBy scrollTopDelta
            , register = V.defaultRegister
            , modeName = getModeName buf.mode
            , recordMacro = Nothing
            , recordKeys = ""
            }


onResize : Size -> Global -> Global
onResize size global =
    let
        size1 =
            { size | height = size.height - (global.statusbarHeight * global.lineHeight) }
    in
    { global
        | window = resizeViews size1 global.lineHeight global.window
        , size = size1
    }
