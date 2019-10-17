module TextBuffer exposing
    ( Edit
    , Patch(..)
    , TextBuffer
    , commit
    , delete
    , emptyEdit
    , fromString
    , insert
    , isDirty
    , iterateLinesBackward
    , iterateLinesForward
    , length
    , lines
    , moveCursorTo
    , moveCursorToPoint
    , redo
    , save
    , toString
    , undo
    )

import History exposing (StateHistory)
import PieceTable as PT exposing (Piece, PieceTable)



--import TreeSitter as TS exposing (Tree)
-- PATCH


type Patch
    = Insertion Int String
    | Deletion Int Int


emptyPatch : Patch
emptyPatch =
    Deletion 0 0


isEmptyPatch : Patch -> Bool
isEmptyPatch p =
    case p of
        Insertion _ "" ->
            True

        Deletion a b ->
            a >= b

        _ ->
            False


stringInsertAt : Int -> String -> String -> String
stringInsertAt i target s =
    String.left i target
        ++ s
        ++ String.dropLeft i target


mergePatch : Patch -> Patch -> List Patch
mergePatch a b =
    (case a of
        Insertion starta sa ->
            let
                enda =
                    starta + String.length sa
            in
            case b of
                Insertion startb sb ->
                    if starta <= startb && startb <= enda then
                        [ Insertion starta <| stringInsertAt (startb - starta) sa sb ]

                    else
                        [ a, b ]

                Deletion startb endb ->
                    if starta <= startb && startb < enda then
                        [ Insertion starta <| String.slice (startb - starta) (endb - starta) sa
                        , Deletion enda endb
                        ]

                    else
                        [ a, b ]

        Deletion starta enda ->
            case b of
                Insertion startb sb ->
                    [ a, b ]

                Deletion startb endb ->
                    if starta == startb then
                        [ Deletion starta (enda + endb - startb) ]

                    else if starta == endb then
                        [ Deletion startb enda ]

                    else
                        [ a, b ]
    )
        |> List.filter (isEmptyPatch >> not)



-- EDIT


type alias Edit =
    { start : Int
    , end : Int
    , oldEnd : Int
    , startPoint : ( Int, Int )
    , endPoint : ( Int, Int )
    , oldEndPoint : ( Int, Int )
    }


emptyEdit : Edit
emptyEdit =
    { start = 0
    , end = 0
    , oldEnd = 0
    , startPoint = ( 0, 0 )
    , endPoint = ( 0, 0 )
    , oldEndPoint = ( 0, 0 )
    }



-- BUFFER


type alias TextBufferState =
    { text : PieceTable
    , cursor : PT.Cursor
    , edits : List Edit
    }


type alias TextBuffer =
    StateHistory TextBufferState Patch


fromString : String -> TextBuffer
fromString s =
    History.empty
        { text = PT.create s
        , cursor = PT.zeroCursor
        , edits = []
        }


toString : TextBuffer -> String
toString buf =
    buf
        |> History.getState
        |> .text
        |> PT.toString


lines : TextBuffer -> Int
lines buf =
    PT.totalLines (History.getState buf).text


length : TextBuffer -> Int
length buf =
    PT.totalLength (History.getState buf).text


applyPatch : Patch -> TextBufferState -> ( TextBufferState, Patch )
applyPatch patch ({ text, cursor } as buf) =
    let
        offset =
            PT.getOffset cursor
    in
    case patch of
        Insertion start s ->
            case PT.cursorAddOffsets (start - offset) text cursor of
                Just startCursor ->
                    let
                        text1 =
                            PT.insert startCursor s text

                        len =
                            String.length s
                    in
                    case PT.cursorAddOffsets len text1 startCursor of
                        Just endCursor ->
                            ( { buf
                                | text = text1
                                , cursor = startCursor
                                , edits =
                                    { start = start
                                    , end = start + len
                                    , oldEnd = start
                                    , startPoint = PT.getPoint text1 startCursor
                                    , endPoint = PT.getPoint text1 endCursor
                                    , oldEndPoint = PT.getPoint text1 startCursor
                                    }
                                        :: buf.edits
                              }
                            , Deletion start (start + len)
                            )

                        _ ->
                            ( buf, emptyPatch )

                _ ->
                    ( buf, emptyPatch )

        Deletion start end ->
            Maybe.map2
                (\startCursor endCursor ->
                    let
                        text1 =
                            PT.delete startCursor endCursor text
                    in
                    ( { buf
                        | text = text1
                        , cursor = endCursor
                        , edits =
                            { start = PT.getOffset startCursor
                            , end = PT.getOffset startCursor
                            , oldEnd = PT.getOffset endCursor
                            , startPoint = PT.getPoint text1 startCursor
                            , endPoint = PT.getPoint text1 startCursor
                            , oldEndPoint = PT.getPoint text1 endCursor
                            }
                                :: buf.edits
                      }
                    , Insertion start <|
                        PT.substring startCursor endCursor buf.text
                    )
                )
                (PT.cursorAddOffsets (start - offset) text cursor)
                (PT.cursorAddOffsets (end - offset) text cursor)
                |> Maybe.withDefault ( buf, emptyPatch )


withEdits : (TextBuffer -> TextBuffer) -> (TextBuffer -> ( TextBuffer, List Edit ))
withEdits fn =
    \buf ->
        let
            buf1 =
                fn buf
        in
        ( History.mapState (\b -> { b | edits = [] }) buf1
        , (History.getState buf1).edits
        )


insert : String -> TextBuffer -> ( TextBuffer, List Edit )
insert s =
    withEdits <| insertHelper s


insertHelper : String -> TextBuffer -> TextBuffer
insertHelper s buf =
    let
        { cursor } =
            History.getState buf
    in
    History.exec applyPatch mergePatch (Insertion (PT.getOffset cursor) s) buf


delete : Int -> TextBuffer -> ( TextBuffer, List Edit )
delete len =
    withEdits <| deleteHelper len


deleteHelper : Int -> TextBuffer -> TextBuffer
deleteHelper len buf =
    let
        { cursor } =
            History.getState buf

        a =
            PT.getOffset cursor

        b =
            a + len
    in
    History.exec applyPatch mergePatch (Deletion a b) buf


commit : TextBuffer -> TextBuffer
commit =
    History.commit


undo : TextBuffer -> ( TextBuffer, List Edit )
undo =
    withEdits <| History.undo applyPatch


redo : TextBuffer -> ( TextBuffer, List Edit )
redo =
    withEdits <| History.redo applyPatch


save : TextBuffer -> TextBuffer
save =
    History.save


isDirty : TextBuffer -> Bool
isDirty =
    History.isDirty


getCursorLine : TextBuffer -> String
getCursorLine buf =
    let
        { text, cursor } =
            History.getState buf
    in
    PT.getLine text cursor


getCursorPoint : TextBuffer -> ( Int, Int )
getCursorPoint buf =
    let
        { cursor, text } =
            History.getState buf
    in
    PT.getPoint text cursor


getCursorOffset : TextBuffer -> Int
getCursorOffset buf =
    PT.getOffset (History.getState buf).cursor


moveCursorTo : Int -> TextBuffer -> TextBuffer
moveCursorTo offset1 =
    History.mapState <|
        \({ text, cursor } as state) ->
            let
                offset =
                    PT.getOffset cursor
            in
            case PT.cursorAddOffsets (offset1 - offset) text cursor of
                Just newCursor ->
                    { state | cursor = newCursor }

                _ ->
                    state


moveCursorToPoint : ( Int, Int ) -> TextBuffer -> TextBuffer
moveCursorToPoint ( row1, col ) =
    History.mapState <|
        \({ text, cursor } as state) ->
            let
                row =
                    PT.getRow cursor
            in
            cursor
                |> PT.cursorAddLines (row1 - row) text
                |> Maybe.andThen
                    (\newCursor ->
                        newCursor
                            |> PT.cursorAddOffsets col text
                            |> Maybe.map
                                (\newCursor1 ->
                                    { state | cursor = newCursor1 }
                                )
                    )
                |> Maybe.withDefault state


iterateLinesForward :
    TextBuffer
    -> (String -> state -> Maybe state)
    -> state
    -> state
iterateLinesForward buf fn state =
    let
        { text, cursor } =
            History.getState buf
    in
    PT.iterateLinesForward cursor text (\line cur _ s -> fn line s) state


iterateLinesBackward :
    TextBuffer
    -> (String -> state -> Maybe state)
    -> state
    -> state
iterateLinesBackward buf fn state =
    let
        { text, cursor } =
            History.getState buf
    in
    PT.iterateLinesBackward cursor text (\line cur _ s -> fn line s) state
