module Model.LoadBuffer exposing
    ( LoadBuffer(..)
    , getBufferId
    , getLoadedBuffer
    , getNotLoadBuffer
    )

import Model.Buffer exposing (Buffer)


type LoadBuffer
    = NotLoad Buffer
    | Loaded Buffer


getLoadedBuffer : LoadBuffer -> Maybe Buffer
getLoadedBuffer buf =
    case buf of
        NotLoad _ ->
            Nothing

        Loaded b ->
            Just b


getNotLoadBuffer : LoadBuffer -> Maybe Buffer
getNotLoadBuffer buf =
    case buf of
        NotLoad b ->
            Just b

        Loaded _ ->
            Nothing


getBufferId : LoadBuffer -> String
getBufferId b =
    case b of
        Loaded buf ->
            buf.path

        NotLoad buf ->
            buf.path
