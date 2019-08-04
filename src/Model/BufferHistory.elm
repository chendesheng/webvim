module Model.BufferHistory exposing
    ( BufferHistory
    , Redo
    , Undo
    , emptyBufferHistory
    , emptyUndo
    , getLastDeleted
    , getLastPatch
    , historyDecoder
    , historyEncoder
    , patchDecoder
    , patchEncoder
    , undoDecoder
    , undoEncoder
    )

import Internal.Position exposing (..)
import Internal.TextBuffer as B exposing (Patch(..), RegionChange, TextBuffer)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Undo =
    { cursor : Position
    , patches : List Patch
    }


emptyUndo : Undo
emptyUndo =
    { patches = [], cursor = ( 0, 0 ) }


type alias Redo =
    Undo


type alias BufferHistory =
    { undoes : List Undo
    , pending : Undo
    , redoes : List Redo
    , savePoint : Int
    , version : Int

    -- changes in current message
    , diff : List RegionChange

    -- from server
    , lastModified : String

    -- for persistent
    , changes : List Patch
    , pendingChanges : List Patch
    }


emptyBufferHistory : BufferHistory
emptyBufferHistory =
    { undoes = []
    , pending = { patches = [], cursor = ( 0, 0 ) }
    , redoes = []
    , savePoint = 0
    , version = 0
    , lastModified = ""
    , changes = []
    , pendingChanges = []
    , diff = []
    }


historyEncoder : BufferHistory -> Encode.Value
historyEncoder { version, savePoint, undoes, redoes, changes, lastModified } =
    Encode.object
        [ ( "version", Encode.int version )
        , ( "savePoint", Encode.int savePoint )
        , ( "undoes", Encode.list undoEncoder undoes )
        , ( "redoes", Encode.list undoEncoder redoes )
        , ( "changes", Encode.list patchEncoder changes )
        , ( "lastModified", Encode.string lastModified )
        ]


historyDecoder : Decode.Decoder BufferHistory
historyDecoder =
    Decode.map6
        (\version savePoint undoes redoes changes lastModified ->
            { emptyBufferHistory
                | undoes = undoes
                , pending = emptyUndo
                , redoes = redoes
                , savePoint = savePoint
                , version = version
                , lastModified = lastModified
                , changes = changes
            }
        )
        (Decode.field "version" Decode.int)
        (Decode.field "savePoint" Decode.int)
        (Decode.field "undoes" <| Decode.list undoDecoder)
        (Decode.field "redoes" <| Decode.list undoDecoder)
        (Decode.field "changes" <| Decode.list patchDecoder)
        (Decode.field "lastModified" Decode.string)


patchEncoder : Patch -> Encode.Value
patchEncoder p =
    case p of
        Deletion b e ->
            Encode.object
                [ ( "type", Encode.string "-" )
                , ( "b", cursorEncoder b )
                , ( "e", cursorEncoder e )
                ]

        Insertion pos s ->
            Encode.object
                [ ( "type", Encode.string "+" )
                , ( "pos", cursorEncoder pos )
                , ( "s", Encode.string <| B.toString s )
                ]


patchDecoder : Decode.Decoder Patch
patchDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typ ->
                if typ == "+" then
                    Decode.map2 Insertion
                        (Decode.field "pos" cursorDecoder)
                        (Decode.field "s" Decode.string
                            |> Decode.map B.fromString
                        )

                else if typ == "-" then
                    Decode.map2 Deletion
                        (Decode.field "b" cursorDecoder)
                        (Decode.field "e" cursorDecoder)

                else
                    Decode.fail <| "unknown type: " ++ typ
            )


undoDecoder : Decode.Decoder Undo
undoDecoder =
    Decode.map2 Undo
        (Decode.field "cursor" <| cursorDecoder)
        (Decode.field "patches" <| Decode.list patchDecoder)


undoEncoder : Undo -> Encode.Value
undoEncoder { cursor, patches } =
    Encode.object
        [ ( "cursor", cursorEncoder cursor )
        , ( "patches", Encode.list patchEncoder patches )
        ]


getLastPatch : BufferHistory -> Maybe Patch
getLastPatch history =
    List.head history.pending.patches


getLastDeleted : BufferHistory -> Maybe TextBuffer
getLastDeleted history =
    getLastPatch history
        |> Maybe.andThen
            (\patch ->
                case patch of
                    Insertion _ s ->
                        Just s

                    Deletion _ _ ->
                        Nothing
            )
