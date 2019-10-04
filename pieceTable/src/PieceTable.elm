module PieceTable exposing
    ( Cursor
    , Piece
    , PieceSource(..)
    , PieceTable
    , create
    , cursorAddLines
    , cursorAddOffsets
    , cursorLineStart
    , delete
    , getLine
    , getOffset
    , getPoint
    , getRow
    , insert
    , isEmpty
    , iterateLinesBackward
    , iterateLinesForward
    , maxCursor
    , substring
    , toString
    , totalLength
    , totalLines
    , zeroCursor
    )

import Array exposing (Array)
import Regex as Re


type PieceSource
    = Original
    | Appends


type alias Piece =
    { source : PieceSource
    , start : Int
    , length : Int
    , lineStarts : Array Int
    }


type alias PieceTable =
    { pieces : Array Piece
    , original : String
    , appends : String
    , length : Int
    , lines : Int

    --, markers : Dict Int Cursor
    }


emptyPiece : Piece
emptyPiece =
    { source = Original
    , start = 0
    , length = 0
    , lineStarts = Array.empty
    }


sentinel : Piece
sentinel =
    { emptyPiece | lineStarts = Array.fromList [ 0 ] }



-- Modify


create : String -> PieceTable
create original =
    let
        lineStarts =
            computeLineStarts original

        length =
            String.length original
    in
    { pieces =
        [ { source = Original
          , start = 0
          , length = length
          , lineStarts = lineStarts
          }
        ]
            |> removeEmptyPieces
            |> Array.fromList
            |> fixSentinel
    , original = original
    , appends = ""
    , lines = Array.length lineStarts
    , length = length
    }


fixSentinel : Array Piece -> Array Piece
fixSentinel pieces =
    case Array.get 0 pieces of
        Just head ->
            if head == sentinel then
                pieces

            else
                prepend sentinel pieces

        _ ->
            Array.fromList [ sentinel ]


fixFirstLineStarts : Array Piece -> Array Piece
fixFirstLineStarts pieces =
    arrayUpdate 0
        (\piece ->
            { piece
                | lineStarts =
                    case Array.get 0 piece.lineStarts of
                        Just 0 ->
                            piece.lineStarts

                        _ ->
                            prepend 0 piece.lineStarts
            }
        )
        pieces


computeLineStarts : String -> Array Int
computeLineStarts s =
    s
        |> Re.find (Maybe.withDefault Re.never <| Re.fromString "\u{000D}?\n")
        |> List.map (\{ match, index } -> index + String.length match)
        |> Array.fromList


insertPiece : { pieceIndex : Int, charIndex : Int } -> Piece -> Array Piece -> Array Piece
insertPiece charPos piece pieces =
    --let
    --_ =
    --Debug.log "insert Piece" ( cursor, piece, pieces )
    --in
    case Array.get charPos.pieceIndex pieces of
        Just p ->
            let
                l =
                    leftPiece charPos.charIndex p

                r =
                    rightPiece charPos.charIndex p

                replacePieces newPieces =
                    pieces
                        |> splice charPos.pieceIndex
                            (charPos.pieceIndex + 1)
                            (removeEmptyPieces newPieces)
            in
            if r.length == 0 then
                replacePieces <| mergePiece l piece

            else if l.length == 0 then
                case Array.get (charPos.pieceIndex - 1) pieces of
                    Just prev ->
                        insertPiece
                            { pieceIndex = charPos.pieceIndex - 1
                            , charIndex = prev.length
                            }
                            piece
                            pieces

                    _ ->
                        replacePieces [ piece, r ]

            else
                replacePieces [ l, piece, r ]

        --|> Debug.log "insert piece result"
        _ ->
            pieces


insert : Cursor -> String -> PieceTable -> PieceTable
insert (Cursor cursor) s ({ pieces, appends, length, lines } as t) =
    let
        lineStarts =
            computeLineStarts s
    in
    { t
        | pieces =
            pieces
                |> insertPiece
                    cursor.charPos
                    { start = String.length appends
                    , length = String.length s
                    , source = Appends
                    , lineStarts = lineStarts
                    }
                |> fixSentinel
        , appends = appends ++ s
        , length = length + String.length s
        , lines = lines + Array.length lineStarts
    }


cursorFieldsPiece : PieceTable -> CursorFields -> Maybe Piece
cursorFieldsPiece t { charPos } =
    Array.get charPos.pieceIndex t.pieces


cursorFieldsMin : CursorFields -> CursorFields -> CursorFields
cursorFieldsMin cur1 cur2 =
    if cur1.charPos.pieceIndex < cur2.charPos.pieceIndex then
        cur1

    else if cur1.charPos.pieceIndex > cur2.charPos.pieceIndex then
        cur2

    else if cur1.charPos.charIndex < cur2.charPos.charIndex then
        cur1

    else
        cur2


cursorFieldsMax : CursorFields -> CursorFields -> CursorFields
cursorFieldsMax cur1 cur2 =
    if cur1.charPos.pieceIndex < cur2.charPos.pieceIndex then
        cur2

    else if cur1.charPos.pieceIndex > cur2.charPos.pieceIndex then
        cur1

    else if cur1.charPos.charIndex < cur2.charPos.charIndex then
        cur2

    else
        cur1


delete : Cursor -> Cursor -> PieceTable -> PieceTable
delete (Cursor from) (Cursor to) ({ pieces, length, lines } as t) =
    let
        maxCur =
            maxCursorFields t

        p1 =
            from
                |> cursorFieldsMax zeroCursorFields
                |> cursorFieldsMin maxCur
                |> cursorFieldsPiece t
                |> Maybe.withDefault emptyPiece

        p2 =
            to
                |> cursorFieldsMax zeroCursorFields
                |> cursorFieldsMin maxCur
                |> cursorFieldsPiece t
                |> Maybe.withDefault emptyPiece

        toRemove =
            Array.slice from.charPos.pieceIndex
                (to.charPos.pieceIndex + 1)
                pieces

        toInsert =
            mergePiece
                (leftPiece from.charPos.charIndex p1)
                (rightPiece to.charPos.charIndex p2)
                |> removeEmptyPieces
                |> Array.fromList

        toInsert1 =
            if from.charPos.pieceIndex == 0 then
                fixSentinel toInsert

            else
                toInsert
    in
    { t
        | pieces =
            splice from.charPos.pieceIndex
                (to.charPos.pieceIndex + 1)
                (Array.toList toInsert1)
                pieces
        , length = length - piecesLength toRemove + piecesLength toInsert1
        , lines = lines - piecesLines toRemove + piecesLines toInsert1
    }


mergePiece : Piece -> Piece -> List Piece
mergePiece a b =
    if a.source == b.source && a.length + b.length < 1000 then
        if a.start + a.length == b.start then
            [ mergePieceInner a b ]

        else if b.start + b.length == a.start then
            [ mergePieceInner b a ]

        else
            [ a, b ]

    else
        [ a, b ]


mergePieceInner : Piece -> Piece -> Piece
mergePieceInner a b =
    { source = a.source
    , start = a.start
    , length = a.length + b.length
    , lineStarts =
        Array.append a.lineStarts <|
            Array.map ((+) a.length) b.lineStarts
    }


fixEmptyPieces : Array Piece -> Array Piece
fixEmptyPieces pieces =
    if Array.isEmpty pieces then
        Array.fromList [ emptyPiece ]

    else
        pieces



-- Sub string


toString : PieceTable -> String
toString t =
    t.pieces
        |> Array.map (getString t)
        |> Array.toList
        |> String.join ""


getLine : PieceTable -> Cursor -> String
getLine t cursor =
    --let
    --    _ =
    --        Debug.log "t" ( t, cursor )
    --in
    let
        cursor1 =
            cursorLineStart t cursor

        --|> Debug.log "lineStart"
    in
    substring
        cursor1
        (cursorAddLines 1 t cursor1
            |> Maybe.withDefault (Cursor <| maxCursorFields t)
         --|> Debug.log "nextlineStart"
        )
        t


iterateLinesForward :
    Cursor
    -> PieceTable
    -> (String -> Cursor -> Cursor -> a -> Maybe a)
    -> a
    -> a
iterateLinesForward (Cursor cursor) t fn =
    iterateLinesForwardHelper
        (cursorFieldsLineStart t cursor)
        t.pieces
        (\cur1 cur2 ->
            fn (substring (Cursor cur1) (Cursor cur2) t)
                (Cursor cur1)
                (Cursor cur2)
        )


iterateLinesForwardHelper :
    CursorFields
    -> Array Piece
    -> (CursorFields -> CursorFields -> a -> Maybe a)
    -> a
    -> a
iterateLinesForwardHelper cursor pieces fn a =
    --let
    --    _ =
    --        Debug.log "iterate forward" cursor
    --in
    case cursorAddLinesHelper 1 pieces cursor of
        Just cursor1 ->
            case fn cursor cursor1 a of
                Just a1 ->
                    iterateLinesForwardHelper cursor1 pieces fn a1

                _ ->
                    a

        _ ->
            fn cursor (maxCursorFieldsHelper pieces) a
                |> Maybe.withDefault a


iterateLinesBackward :
    Cursor
    -> PieceTable
    -> (String -> Cursor -> Cursor -> a -> Maybe a)
    -> a
    -> a
iterateLinesBackward (Cursor cursor) t fn =
    iterateLinesBackwardHelper
        (cursorFieldsLineStart t cursor)
        t.pieces
        (\cur1 cur2 ->
            fn (substring (Cursor cur1) (Cursor cur2) t)
                (Cursor cur1)
                (Cursor cur2)
        )


iterateLinesBackwardHelper :
    CursorFields
    -> Array Piece
    -> (CursorFields -> CursorFields -> a -> Maybe a)
    -> a
    -> a
iterateLinesBackwardHelper cursor pieces fn a =
    --let
    --    _ =
    --        Debug.log "iterate backward" ( cursor, pieces )
    --in
    case cursorAddLinesHelper -1 pieces cursor of
        Just cursor1 ->
            case fn cursor1 cursor a of
                Just a1 ->
                    iterateLinesBackwardHelper cursor1 pieces fn a1

                _ ->
                    a

        _ ->
            a



--|> Debug.log "getLine resp"


isEmpty : PieceTable -> Bool
isEmpty t =
    Array.length t.pieces == 1


totalLines : PieceTable -> Int
totalLines t =
    t.lines + 1


totalLength : PieceTable -> Int
totalLength t =
    t.length


lengthHelper : Int -> Int -> Array Piece -> Int
lengthHelper i j pieces =
    case Array.get i pieces of
        Just p ->
            if i < j then
                p.length + lengthHelper (i + 1) j pieces

            else
                0

        _ ->
            0



-- Cursor


type Cursor
    = Cursor CursorFields


type alias CursorFields =
    { charPos :
        { pieceIndex : Int
        , charIndex : Int
        }
    , linePos :
        { pieceIndex : Int
        , lineIndex : Int
        }
    , offset : Int -- absolute char offset
    , row : Int -- absolute line number
    }


zeroCursor : Cursor
zeroCursor =
    Cursor zeroCursorFields


zeroCursorFields : CursorFields
zeroCursorFields =
    { charPos = { pieceIndex = 0, charIndex = 0 }
    , linePos = { pieceIndex = 0, lineIndex = 0 }
    , offset = 0
    , row = 0
    }


getRow : Cursor -> Int
getRow (Cursor { row, linePos }) =
    row + linePos.lineIndex + 1


getPoint : PieceTable -> Cursor -> ( Int, Int )
getPoint { pieces, lines, length } (Cursor { row, offset, linePos, charPos }) =
    ( row + linePos.lineIndex + 1
    , if linePos.pieceIndex < charPos.pieceIndex then
        (pieces
            |> Array.slice linePos.pieceIndex charPos.pieceIndex
            |> piecesLength
        )
            - (Array.get linePos.pieceIndex pieces
                |> Maybe.andThen (\piece -> arrayLastItem piece.lineStarts)
                |> Maybe.withDefault 0
              )
            + charPos.charIndex

      else
        Array.get linePos.pieceIndex pieces
            |> Maybe.andThen
                (\piece ->
                    piece.lineStarts
                        |> Array.get linePos.lineIndex
                        |> Maybe.map (\lineStart -> charPos.charIndex - lineStart)
                )
            |> Maybe.withDefault 0
    )


getOffset : Cursor -> Int
getOffset (Cursor { offset, charPos }) =
    offset + charPos.charIndex


maxCursor : PieceTable -> Cursor
maxCursor t =
    Cursor <| maxCursorFields t


maxCursorFields : PieceTable -> CursorFields
maxCursorFields { pieces } =
    maxCursorFieldsHelper pieces


maxCursorFieldsHelper : Array Piece -> CursorFields
maxCursorFieldsHelper pieces =
    { charPos = { pieceIndex = Array.length pieces, charIndex = 0 }
    , linePos = { pieceIndex = Array.length pieces, lineIndex = 0 }
    , offset = 0
    , row = 0
    }


cursorAddLines : Int -> PieceTable -> Cursor -> Maybe Cursor
cursorAddLines cnt t (Cursor cursor) =
    cursor
        |> cursorFieldsAddLines cnt t
        |> Maybe.map Cursor


cursorFieldsAddLines : Int -> PieceTable -> CursorFields -> Maybe CursorFields
cursorFieldsAddLines cnt t cursor =
    cursorAddLinesHelper cnt t.pieces cursor
        --|> Debug.log "cursorAddLinesHelper"
        |> Maybe.map (updateCursorFields t.pieces cursor)



--cursorFieldsAddLines : Int -> Array Piece ->


cursorAddLinesHelper : Int -> Array Piece -> CursorFields -> Maybe CursorFields
cursorAddLinesHelper cnt pieces { linePos, charPos, row, offset } =
    if cnt > 0 then
        addLinesHelper (linePos.lineIndex + cnt)
            pieces
            linePos.pieceIndex

    else if linePos.lineIndex + cnt >= 0 then
        --let
        --    _ =
        --        Debug.log "pieces, linePos" ( pieces, linePos )
        --in
        -- move back and still in the same piece
        case Array.get linePos.pieceIndex pieces of
            Just piece ->
                case Array.get (linePos.lineIndex + cnt) piece.lineStarts of
                    Just charIndex ->
                        Just
                            { linePos =
                                { pieceIndex = linePos.pieceIndex
                                , lineIndex = linePos.lineIndex + cnt
                                }
                            , charPos =
                                { pieceIndex = linePos.pieceIndex
                                , charIndex = charIndex
                                }
                            , row = row
                            , offset = offset
                            }

                    _ ->
                        Nothing

            _ ->
                Nothing

    else
        -- move back to previous piece
        subLinesHelper
            (-cnt - linePos.lineIndex)
            pieces
            (linePos.pieceIndex - 1)


addLinesHelper : Int -> Array Piece -> Int -> Maybe CursorFields
addLinesHelper cnt pieces pieceIndex =
    case Array.get pieceIndex pieces of
        Just piece ->
            case Array.get cnt piece.lineStarts of
                Just charIndex ->
                    Just
                        { linePos = { pieceIndex = pieceIndex, lineIndex = cnt }
                        , charPos = { pieceIndex = pieceIndex, charIndex = charIndex }
                        , offset = 0
                        , row = 0
                        }

                _ ->
                    addLinesHelper (cnt - Array.length piece.lineStarts)
                        pieces
                        (pieceIndex + 1)

        _ ->
            Nothing


subLinesHelper : Int -> Array Piece -> Int -> Maybe CursorFields
subLinesHelper cnt pieces pieceIndex =
    case Array.get pieceIndex pieces of
        Just piece ->
            let
                n =
                    Array.length piece.lineStarts
            in
            case Array.get (n - cnt) piece.lineStarts of
                Just charIndex ->
                    Just
                        { linePos = { pieceIndex = pieceIndex, lineIndex = n - cnt }
                        , charPos = { pieceIndex = pieceIndex, charIndex = charIndex }
                        , offset = 0
                        , row = 0
                        }

                _ ->
                    subLinesHelper (cnt - n)
                        pieces
                        (pieceIndex - 1)

        _ ->
            if pieceIndex == -1 && cnt == 0 then
                Just zeroCursorFields

            else
                Nothing


cursorLineStart : PieceTable -> Cursor -> Cursor
cursorLineStart t cursor =
    cursor
        |> cursorAddLines 0 t
        |> Maybe.withDefault cursor


cursorFieldsLineStart : PieceTable -> CursorFields -> CursorFields
cursorFieldsLineStart t cursor =
    cursor
        |> cursorFieldsAddLines 0 t
        |> Maybe.withDefault cursor


piecesLines : Array Piece -> Int
piecesLines =
    Array.foldl
        (\piece result ->
            result + Array.length piece.lineStarts
        )
        0


piecesLength : Array Piece -> Int
piecesLength =
    Array.foldl
        (\piece result ->
            result + piece.length
        )
        0


updateCursorFields : Array Piece -> CursorFields -> CursorFields -> CursorFields
updateCursorFields pieces from to =
    let
        length =
            if from.charPos.pieceIndex < to.charPos.pieceIndex then
                pieces
                    |> Array.slice from.charPos.pieceIndex to.charPos.pieceIndex
                    |> piecesLength

            else
                -(pieces
                    |> Array.slice to.charPos.pieceIndex from.charPos.pieceIndex
                    |> piecesLength
                 )

        lines =
            if from.linePos.pieceIndex < to.linePos.pieceIndex then
                pieces
                    |> Array.slice from.linePos.pieceIndex to.linePos.pieceIndex
                    |> piecesLines

            else
                -(pieces
                    |> Array.slice to.linePos.pieceIndex from.linePos.pieceIndex
                    |> piecesLines
                 )
    in
    { to | row = from.row + lines, offset = from.offset + length }


cursorAddOffsets : Int -> PieceTable -> Cursor -> Maybe Cursor
cursorAddOffsets cnt t (Cursor cursor) =
    cursor
        |> cursorFieldsAddOffsets cnt t.pieces
        |> Maybe.map (updateCursorFields t.pieces cursor >> Cursor)


cursorFieldsAddOffsets : Int -> Array Piece -> CursorFields -> Maybe CursorFields
cursorFieldsAddOffsets cnt pieces cursor =
    cursorFieldsAddOffsetsHelper cnt pieces cursor
        |> Maybe.map (updateCursorFields pieces cursor)


cursorFieldsAddOffsetsHelper : Int -> Array Piece -> CursorFields -> Maybe CursorFields
cursorFieldsAddOffsetsHelper cnt pieces cursor =
    if cnt == 0 then
        Just cursor

    else if cnt > 0 then
        addOffsetsHelper pieces
            cursor.charPos.pieceIndex
            (cursor.charPos.charIndex + cnt)

    else if cursor.charPos.charIndex + cnt >= 0 then
        let
            charPos =
                { charIndex = cursor.charPos.charIndex + cnt
                , pieceIndex = cursor.charPos.pieceIndex
                }

            linePos =
                getLinePos pieces charPos
        in
        Just
            { cursor
                | charPos = charPos
                , linePos = linePos
                , offset = cursor.offset
                , row = cursor.row
            }

    else
        subOffsetsHelper pieces
            (cursor.charPos.pieceIndex - 1)
            (-cnt - cursor.charPos.charIndex)


getLinePos :
    Array Piece
    -> { pieceIndex : Int, charIndex : Int }
    -> { pieceIndex : Int, lineIndex : Int }
getLinePos pieces { pieceIndex, charIndex } =
    case Array.get pieceIndex pieces of
        Just piece ->
            case Array.get 0 piece.lineStarts of
                Just i ->
                    if charIndex < i then
                        getLinePosHelper pieces (pieceIndex - 1)

                    else
                        { pieceIndex = pieceIndex
                        , lineIndex =
                            piece.lineStarts
                                |> findItem (\j -> j > charIndex)
                                |> Maybe.map
                                    (\( lineIndex1, lineStart ) -> max 0 <| lineIndex1 - 1)
                                |> Maybe.withDefault 0
                        }

                _ ->
                    getLinePosHelper pieces (pieceIndex - 1)

        _ ->
            { pieceIndex = 0, lineIndex = 0 }


getLinePosHelper : Array Piece -> Int -> { pieceIndex : Int, lineIndex : Int }
getLinePosHelper pieces i =
    case Array.get i pieces of
        Just piece ->
            if Array.isEmpty piece.lineStarts then
                getLinePosHelper pieces (i - 1)

            else
                { pieceIndex = i
                , lineIndex = Array.length piece.lineStarts - 1
                }

        _ ->
            { pieceIndex = 0, lineIndex = 0 }


addOffsetsHelper : Array Piece -> Int -> Int -> Maybe CursorFields
addOffsetsHelper pieces pieceIndex cnt =
    case Array.get pieceIndex pieces of
        Just piece ->
            if piece.length == 0 then
                addOffsetsHelper pieces (pieceIndex + 1) cnt

            else if cnt > piece.length then
                addOffsetsHelper pieces
                    (pieceIndex + 1)
                    (cnt - piece.length)

            else
                let
                    charPos =
                        { pieceIndex = pieceIndex, charIndex = cnt }

                    linePos =
                        getLinePos pieces charPos
                in
                Just
                    { charPos = charPos
                    , linePos = linePos
                    , row = 0
                    , offset = 0
                    }

        _ ->
            Nothing


subOffsetsHelper : Array Piece -> Int -> Int -> Maybe CursorFields
subOffsetsHelper pieces pieceIndex cnt =
    case Array.get pieceIndex pieces of
        Just piece ->
            let
                len =
                    piece.length
            in
            if cnt > len then
                subOffsetsHelper
                    pieces
                    (pieceIndex - 1)
                    (cnt - len)

            else
                let
                    charPos =
                        { pieceIndex = pieceIndex, charIndex = len - cnt }

                    linePos =
                        getLinePos pieces charPos
                in
                Just
                    { charPos = charPos
                    , linePos = linePos
                    , row = 0
                    , offset = 0
                    }

        _ ->
            Nothing


getString : PieceTable -> Piece -> String
getString { original, appends } { source, start, length } =
    if length == 0 then
        ""

    else
        case source of
            Original ->
                String.slice start (start + length) original

            Appends ->
                String.slice start (start + length) appends


removeEmptyPieces : List Piece -> List Piece
removeEmptyPieces =
    List.filter (\{ length } -> length > 0)


concatArrays3 : Array a -> Array a -> Array a -> Array a
concatArrays3 a1 a2 a3 =
    Array.append (Array.append a1 a2) a3


splice : Int -> Int -> List a -> Array a -> Array a
splice i j inserts array =
    concatArrays3
        (Array.slice 0 i array)
        (Array.fromList inserts)
        (Array.slice j (Array.length array) array)


leftPiece : Int -> Piece -> Piece
leftPiece i p =
    { p
        | length = i
        , lineStarts = Array.filter (\j -> j <= i) p.lineStarts
    }


rightPiece : Int -> Piece -> Piece
rightPiece i p =
    if i == 0 then
        case Array.get 0 p.lineStarts of
            Just 0 ->
                { p
                    | lineStarts = Array.slice 1 (Array.length p.lineStarts) p.lineStarts
                }

            _ ->
                p

    else
        { p
            | length = p.length - i
            , start = p.start + i
            , lineStarts =
                p.lineStarts
                    |> Array.filter (\j -> j > i)
                    |> Array.map (\j -> j - i)
        }


findItem : (a -> Bool) -> Array a -> Maybe ( Int, a )
findItem =
    findItemHelper 0


findItemHelper : Int -> (a -> Bool) -> Array a -> Maybe ( Int, a )
findItemHelper i pred array =
    case Array.get i array of
        Just item ->
            if pred item then
                Just ( i, item )

            else
                findItemHelper (i + 1) pred array

        _ ->
            Nothing


substring : Cursor -> Cursor -> PieceTable -> String
substring (Cursor cur1) (Cursor cur2) t =
    if cur1.charPos.pieceIndex == cur2.charPos.pieceIndex then
        t.pieces
            |> Array.get cur1.charPos.pieceIndex
            |> Maybe.map
                (\piece ->
                    getString t
                        { piece
                            | start = piece.start + cur1.charPos.charIndex
                            , length = cur2.charPos.charIndex - cur1.charPos.charIndex
                        }
                )
            |> Maybe.withDefault ""

    else
        (t.pieces
            |> Array.get cur1.charPos.pieceIndex
            |> Maybe.map
                (\p1 ->
                    p1
                        |> rightPiece cur1.charPos.charIndex
                        |> getString t
                )
            |> Maybe.withDefault ""
         --|> Debug.log "left"
        )
            ++ (t.pieces
                    |> Array.slice (cur1.charPos.pieceIndex + 1) cur2.charPos.pieceIndex
                    |> Array.map (getString t)
                    |> Array.toList
                    |> String.join ""
                --|> Debug.log "middle"
               )
            ++ (t.pieces
                    |> Array.get cur2.charPos.pieceIndex
                    |> Maybe.map
                        (\p1 ->
                            p1
                                |> leftPiece cur2.charPos.charIndex
                                --|> Debug.log "leftPiece"
                                |> getString t
                         --|> Debug.log "leftPiece getString"
                        )
                    |> Maybe.withDefault ""
                --|> Debug.log "right"
               )


prepend : a -> Array a -> Array a
prepend x xs =
    Array.append (Array.push x Array.empty) xs


arrayUpdate : Int -> (a -> a) -> Array a -> Array a
arrayUpdate i fn array =
    case Array.get i array of
        Just a ->
            Array.set i (fn a) array

        _ ->
            array


arrayLastItem : Array a -> Maybe a
arrayLastItem arr =
    Array.get (Array.length arr - 1) arr
