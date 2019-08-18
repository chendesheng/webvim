module TestViewLines exposing (applyPatches, resize, suite)

import Expect exposing (Expectation)
import Fuzz
import Helper.Helper exposing (rangeCount)
import Internal.TextBuffer exposing (Patch(..), RegionChange(..), fromString)
import Model exposing (Editor, updateBuffer)
import Model.Buffer exposing (..)
import Model.Global exposing (..)
import Model.View exposing (..)
import Test exposing (..)
import TextBuffer exposing (..)
import Update exposing (..)
import Update.Buffer exposing (..)
import Update.Vim exposing (applyDiff)


resize : Int -> Editor -> Editor
resize height ({ buf, global } as ed) =
    let
        view =
            buf.view
    in
    { ed
        | buf =
            { buf
                | view =
                    { view
                        | lines = rangeCount 0 (height + 2)
                        , size = { height = height, width = 1 }
                    }
            }
    }


applyPatches : Int -> List Patch -> Expectation
applyPatches height patches =
    let
        { buf, global } =
            { buf = emptyBuffer
            , global = emptyGlobal
            }
                |> resize height
                |> updateBuffer (transaction patches)
                |> applyDiff
    in
    Expect.equal
        (rangeCount buf.view.scrollTop (buf.view.size.height + 2))
        (List.sort buf.view.lines)


suite : Test
suite =
    describe "view lines"
        [ fuzz2
            (Fuzz.intRange 1 5)
            (Fuzz.list fuzzPatch)
            "sorted viewLines should always equal to lines"
            applyPatches
        , describe "scrollViewLines" <|
            [ test "scroll down" <|
                \_ ->
                    List.range 3 6
                        |> scrollViewLines 2 3 5
                        |> Expect.equal [ 7, 8, 5, 6 ]
            , test "scroll up" <|
                \_ ->
                    List.range 3 6
                        |> scrollViewLines 2 3 0
                        |> Expect.equal [ 3, 0, 1, 2 ]
            ]
        , describe "applyRegionChangeToView"
            [ test "remove after scrollBottom" <|
                \_ ->
                    List.range 10 40
                        |> applyRegionChangeToView (RegionRemove ( ( 35, 0 ), ( 90, 0 ) )) 10 30
                        |> Expect.equal (List.range 10 40)
            , test "remove 0 lines" <|
                \_ ->
                    List.range 10 40
                        |> applyRegionChangeToView (RegionRemove ( ( 10, 0 ), ( 10, 0 ) )) 10 30
                        |> Expect.equal (List.range 10 40)
            , test "remove before scrollTop" <|
                \_ ->
                    List.range 10 40
                        |> applyRegionChangeToView (RegionRemove ( ( 0, 0 ), ( 5, 0 ) )) 10 30
                        |> List.sort
                        |> Expect.equal (List.range 5 35)
            , test "remove across scrollTop" <|
                \_ ->
                    -- diff scrollTop height
                    List.range 3 6
                        |> applyRegionChangeToView (RegionRemove ( ( 0, 0 ), ( 5, 0 ) )) 3 4
                        |> Expect.equal [ 2, 3, 0, 1 ]
            , test "remove across scrollBottom" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionRemove ( ( 5, 0 ), ( 7, 0 ) )) 3 4
                        |> Expect.equal [ 3, 4, 5, 6 ]
            , test "remove all view lines" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionRemove ( ( 3, 0 ), ( 7, 0 ) )) 3 4
                        |> Expect.equal [ 3, 4, 5, 6 ]
            , test "remove all lines" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionRemove ( ( 0, 0 ), ( 100, 0 ) )) 3 4
                        |> Expect.equal [ 0, 1, 2, 3 ]
            , test "remove inside view" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionRemove ( ( 4, 0 ), ( 6, 0 ) )) 3 4
                        |> Expect.equal [ 3, 5, 6, 4 ]
            , test "add before scrollTop" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionAdd ( ( 0, 0 ), ( 1, 1 ) )) 3 4
                        |> Expect.equal [ 4, 5, 6, 3 ]
            , test "add after scrollBottom" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionAdd ( ( 100, 0 ), ( 200, 0 ) )) 3 4
                        |> Expect.equal [ 3, 4, 5, 6 ]
            , test "add inside view" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionAdd ( ( 4, 0 ), ( 6, 0 ) )) 3 4
                        |> Expect.equal [ 3, 6, 4, 5 ]
            , test "add more than view" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionAdd ( ( 2, 0 ), ( 100, 0 ) )) 3 4
                        |> Expect.equal [ 3, 4, 5, 6 ]
            , test "add and remove" <|
                \_ ->
                    List.range 3 6
                        |> applyRegionChangeToView (RegionAdd ( ( 2, 0 ), ( 4, 0 ) )) 3 4
                        |> applyRegionChangeToView (RegionRemove ( ( 2, 0 ), ( 3, 0 ) )) 3 4
                        |> Expect.equal [ 4, 5, 2, 3 ]
            ]
        , test "new line" <|
            \_ ->
                applyPatches
                    1
                    [ Insertion ( 0, 0 ) (fromString "1")
                    , Insertion ( 0, 1 ) (fromString "\n")
                    ]
        , test "debugging" <|
            \_ ->
                applyPatches
                    5
                    [ Insertion ( 0, 0 ) (fromString "\n")
                    , Deletion ( 0, 0 ) ( 0, 1 )
                    ]
        ]
