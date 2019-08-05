module Model.Global exposing
    ( Global
    , RegisterText(..)
    , emptyGlobal
    , getBuffers
    , getJumps
    , persistentAll
    , updateJumps
    )

import Debouncers exposing (Debouncers, emptyDebouncers)
import Dict exposing (Dict)
import Font exposing (FontInfo)
import Ime exposing (IME, emptyIme)
import Internal.Jumps exposing (..)
import Internal.Position exposing (..)
import Internal.Window as Win exposing (Window)
import Model.Buffer exposing (Buffer)
import Model.Frame as Frame exposing (Frame)
import Model.Lint exposing (..)
import Model.LoadBuffer exposing (..)
import Model.Size exposing (Size, emptySize)
import Vim.AST as V exposing (VisualType(..))
import Zipper


type RegisterText
    = Text String
    | Lines String


type alias Global =
    { registers : Dict String RegisterText
    , ime : IME
    , dotRegister : String

    {-
       1. change active view and buffer
       2. keep view change active buffer
    -}
    , window : Window Frame
    , buffers : Dict String LoadBuffer
    , cwd : String
    , exHistory : List String
    , searchHistory : List String
    , service : String
    , pathSeperator : String
    , fontInfo : FontInfo
    , homedir : String
    , vimASTCache : Dict ( String, String ) ( V.AST, String )
    , size : Size
    , last :
        { matchChar :
            Maybe
                { char : String
                , before : Bool
                , forward : Bool
                }
        , matchString : Maybe ( String, Bool )
        , inserts : String
        , visual : String
        , ex : String
        , jumpToTag : Maybe Location
        }
    , lint : BufferLint
    , locationList : List Location
    , statusbarHeight : Int
    , showTip : Bool
    , lineHeight : Int
    , theme : String

    -- move window and buffers to here once you need to persistent them to the session storage
    -- this is for performance so you don't need save it on every type
    , persistent :
        Maybe
            { window : Window Frame
            , buffers : List Buffer
            }
    , debouncers : Debouncers
    }


emptyGlobal : Global
emptyGlobal =
    { size = emptySize
    , window = Win.empty
    , dotRegister = ""
    , ime = emptyIme
    , buffers = Dict.empty
    , cwd = ""
    , exHistory = []
    , service = ""
    , pathSeperator = "/"
    , fontInfo =
        { widths = []
        , lineHeight = 0
        , size = 0
        , name = ""
        }
    , homedir = ""
    , registers = Dict.empty
    , searchHistory = []
    , last =
        { matchChar = Nothing
        , matchString = Nothing
        , inserts = ""
        , visual = ""
        , ex = ""
        , jumpToTag = Nothing
        }
    , vimASTCache = Dict.empty

    -- TODO: add a location pool
    -- locations : Dict BufferId (Dict Int Position)
    , lint = { items = [], count = 0 }
    , locationList = []
    , showTip = False
    , statusbarHeight = 1
    , lineHeight = 21
    , persistent = Nothing
    , theme = ""
    , debouncers = emptyDebouncers
    }


getBuffers : Dict String LoadBuffer -> List Buffer
getBuffers =
    let
        buffer buf =
            case buf of
                NotLoad b ->
                    b

                Loaded b ->
                    b
    in
    Dict.values >> List.map buffer


persistentAll : Global -> Global
persistentAll global =
    { global
        | persistent =
            Just
                { window = global.window
                , buffers = getBuffers global.buffers
                }
    }


updateJumps : (Jumps -> Jumps) -> Global -> Global
updateJumps fn global =
    { global
        | window =
            Win.updateActiveFrame
                (Frame.updateJumps fn)
                global.window
    }


getJumps : Global -> Jumps
getJumps global =
    global.window
        |> Win.getActiveFrame
        |> Maybe.map Frame.getJumps
        |> Maybe.withDefault Zipper.empty
