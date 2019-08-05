module Update.Mouse exposing (onMouseClick, onMouseWheel, onResize)

import Internal.TextBuffer as B exposing (Patch(..))
import Internal.Window as Win
import Model exposing (..)
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.Size exposing (..)
import Model.View exposing (..)
import Update.Message exposing (..)
import Update.Vim exposing (..)
import Vim.AST as V exposing (Operator(..))


onMouseWheel : Win.Path -> Int -> Int -> Editor -> ( Editor, Cmd Msg )
onMouseWheel path deltaY deltaX ({ buf, global } as ed) =
    let
        --_ =
        --    Debug.log "deltaY" deltaY
        --_ =
        --    Debug.log "scrollTopPx" scrollTopPx
        --_ =
        --    Debug.log "scrollTopOffsetPx" (remainderBy lineHeight scrollTopPx)
        view =
            buf.view

        lineHeight =
            global.lineHeight

        scrollTopPx =
            (view.scrollTop * lineHeight + view.scrollTopOffsetPx + deltaY)
                |> max 0
                |> min ((B.count buf.lines - 2) * lineHeight)

        scrollTopDelta =
            scrollTopPx // lineHeight - buf.view.scrollTop

        ( ed1, cmd ) =
            applyVimAST False
                "<mousewheel>"
                { count = Nothing
                , edit = Just <| V.Scroll <| V.ScrollBy scrollTopDelta
                , register = V.defaultRegister
                , modeName = getModeName buf.mode
                , recordMacro = Nothing
                , recordKeys = ""
                }
                ed

        --_ =
        --    Debug.log "ed1.buf.view" ed1.buf.view
        view1 =
            ed1.buf.view
    in
    ( { ed1
        | buf =
            { buf
                | view =
                    { view1
                        | scrollTopOffsetPx = remainderBy lineHeight scrollTopPx

                        --, scrollLeftPx =
                        --    (view.scrollLeftPx + deltaX)
                        --        |> max 0
                    }
            }
      }
    , cmd
    )


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


onMouseClick : Win.Path -> Global -> Global
onMouseClick path ({ window } as global) =
    { global
        | window =
            window
                |> Win.getFrame path
                |> Maybe.map
                    (\frame -> Win.setActive ((==) frame) window)
                |> Maybe.withDefault window
    }
