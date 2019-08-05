module Internal.Window exposing
    ( Direction(..)
    , Path
    , Rect
    , Window
    , WindowSplit(..)
    , activeBottomFrame
    , activeLeftFrame
    , activeNextFrame
    , activePrevFrame
    , activeRightFrame
    , activeTopFrame
    , empty
    , getActiveFrame
    , getFrame
    , hsplit
    , initWindow
    , mapFrame
    , removeCurrent
    , setActive
    , setSize
    , toBorders
    , toList
    , toString
    , updateActiveFrame
    , updateFrame
    , vsplit
    , windowDecoder
    , windowEncoder
    )

import Helper.Helper exposing (dropWhile)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Tree


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


type Direction
    = LeftChild
    | RightChild


type alias Path =
    List Direction


type alias Zipper a =
    { tree : Tree a
    , current : Tree a
    , path : Path
    }


{-| return subtree and path to the subtree
-}
searchTree :
    Bool
    -> (a -> Bool)
    -> Tree a
    -> Path
    -> Maybe ( Tree a, Path )
searchTree includeCurrent pred tree path =
    case tree of
        Node a left right ->
            if pred a && includeCurrent then
                Just ( tree, path )

            else
                case searchTree True pred left (LeftChild :: path) of
                    Nothing ->
                        searchTree True pred right (RightChild :: path)

                    res ->
                        res

        _ ->
            Nothing


{-| find start from current position (include current position)
-}
find : Bool -> (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find includeCurrent pred ({ tree, current, path } as z) =
    case current of
        Node a _ right ->
            case searchTree includeCurrent pred current [] of
                Nothing ->
                    case
                        path
                            |> dropWhile ((==) RightChild)
                            |> List.tail
                            |> Maybe.andThen
                                (\path1 ->
                                    subtree (List.reverse path1) tree
                                        |> Maybe.map (Tuple.pair path1)
                                )
                    of
                        Just ( path1, tree1 ) ->
                            case tree1 of
                                Node _ _ right1 ->
                                    find True
                                        pred
                                        { tree = tree
                                        , current = right1
                                        , path = RightChild :: path1
                                        }

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                Just ( tree1, path1 ) ->
                    Just
                        { tree = tree
                        , current = tree1
                        , path = path1 ++ path
                        }

        Empty ->
            Nothing


subtree : Path -> Tree a -> Maybe (Tree a)
subtree path tree =
    case path of
        dir :: rest ->
            case tree of
                Node _ left right ->
                    case dir of
                        LeftChild ->
                            subtree rest left

                        RightChild ->
                            subtree rest right

                _ ->
                    Nothing

        _ ->
            Just tree


updateSubtree : (Tree a -> Tree a) -> Zipper a -> Zipper a
updateSubtree fn ({ tree, current, path } as zipper) =
    let
        path1 =
            List.reverse path

        tree1 =
            updateSubtreeHelper fn path1 tree
    in
    { zipper
        | tree = tree1
        , current =
            subtree path1 tree1
                |> Maybe.withDefault Empty
    }


updateSubtreeHelper : (Tree a -> Tree a) -> Path -> Tree a -> Tree a
updateSubtreeHelper fn path tree =
    case path of
        dir :: rest ->
            case dir of
                LeftChild ->
                    case tree of
                        Node a left right ->
                            Node a
                                (updateSubtreeHelper fn rest left)
                                right

                        _ ->
                            tree

                RightChild ->
                    case tree of
                        Node a left right ->
                            Node a
                                left
                                (updateSubtreeHelper fn rest right)

                        _ ->
                            tree

        _ ->
            fn tree


goRoot : Zipper a -> Zipper a
goRoot { tree, current, path } =
    { tree = tree
    , current = tree
    , path = []
    }


goLeft : Zipper a -> Maybe (Zipper a)
goLeft { tree, path, current } =
    case current of
        Node _ left _ ->
            Just
                { tree = tree
                , path = LeftChild :: path
                , current = left
                }

        _ ->
            Nothing


goParent : Zipper a -> Maybe (Zipper a)
goParent { tree, path, current } =
    case path of
        [] ->
            Nothing

        _ :: ancests ->
            tree
                |> subtree (List.reverse ancests)
                |> Maybe.map
                    (\tree1 ->
                        { tree = tree
                        , current = tree1
                        , path = ancests
                        }
                    )


goRight : Zipper a -> Maybe (Zipper a)
goRight { tree, path, current } =
    case current of
        Node _ _ right ->
            Just
                { tree = tree
                , path = RightChild :: path
                , current = right
                }

        _ ->
            Nothing


isRoot : Zipper a -> Bool
isRoot { path } =
    List.isEmpty path



-- Window


type WindowSplit a
    = HSplit Float
    | VSplit Float
    | NoSplit a


type alias Window a =
    Zipper (WindowSplit a)


empty : Window a
empty =
    { tree = Empty, current = Empty, path = [] }


initWindow : a -> Window a
initWindow frame =
    let
        tree =
            Node (NoSplit frame) Empty Empty
    in
    { tree = tree
    , current = tree
    , path = []
    }


getActiveFrame : Window a -> Maybe a
getActiveFrame { current } =
    case current of
        Node (NoSplit v) _ _ ->
            Just v

        _ ->
            Nothing


goToFrame : (a -> Bool) -> Window a -> Window a
goToFrame pred win =
    win
        |> find True
            (\node ->
                case node of
                    NoSplit frame ->
                        pred frame

                    _ ->
                        False
            )
        |> Maybe.withDefault win


vsplit : Float -> a -> Window a -> Window a
vsplit percent id win =
    split (VSplit (1 - max percent 0.05)) id win


hsplit : Float -> a -> Window a -> Window a
hsplit percent id win =
    split (HSplit (1 - max percent 0.05)) id win


split : WindowSplit a -> a -> Window a -> Window a
split sp frame win =
    win
        |> updateSubtree
            (\tree ->
                case tree of
                    Node w _ _ ->
                        case w of
                            NoSplit id ->
                                Node sp
                                    tree
                                    (Node (NoSplit frame)
                                        Empty
                                        Empty
                                    )

                            o ->
                                tree

                    _ ->
                        tree
            )
        |> goLeft
        |> Maybe.withDefault win


{-| return nothing if it is the last one
-}
removeCurrent : Window a -> Window a
removeCurrent win =
    case win.tree of
        Node (NoSplit _) Empty Empty ->
            win

        _ ->
            let
                isLeftChild =
                    case win.path of
                        LeftChild :: _ ->
                            True

                        _ ->
                            False
            in
            win
                |> goParent
                |> Maybe.map
                    (updateSubtree
                        (\tree1 ->
                            case tree1 of
                                Node _ left right ->
                                    if isLeftChild then
                                        right

                                    else
                                        left

                                _ ->
                                    tree1
                        )
                        >> activeNextFrameHelper True
                    )
                |> Maybe.withDefault win


activeNextFrame : Window a -> Window a
activeNextFrame win =
    activeNextFrameHelper False win


activeNextFrameHelper : Bool -> Window a -> Window a
activeNextFrameHelper includeCurrent win =
    let
        findFrame =
            find includeCurrent
                (\nd ->
                    case nd of
                        NoSplit v ->
                            True

                        _ ->
                            False
                )
    in
    case findFrame win of
        Nothing ->
            if isRoot win then
                -- do nothing if already root
                win

            else
                win
                    |> goRoot
                    |> findFrame
                    |> Maybe.withDefault win

        Just win1 ->
            win1


activePrevFrame : Window a -> Window a
activePrevFrame win =
    let
        isLeftChild win1 =
            case win1.path of
                LeftChild :: _ ->
                    True

                _ ->
                    False

        findFrame win1 =
            let
                win2 =
                    goParent win1
            in
            if isLeftChild win1 then
                Maybe.andThen findFrame win2

            else
                Maybe.andThen goLeft win2
    in
    case findFrame win of
        Nothing ->
            if isRoot win then
                -- do nothing if already root
                win

            else
                win
                    |> goRoot
                    |> goRightMost

        Just win1 ->
            goRightMost win1


activeRightFrame : Window a -> Window a
activeRightFrame win =
    activeRightFrameHelper (LeftChild :: win.path) win
        |> Maybe.withDefault win


activeLeftFrame : Window a -> Window a
activeLeftFrame win =
    activeLeftFrameHelper (LeftChild :: win.path) win
        |> Maybe.withDefault win


activeBottomFrame : Window a -> Window a
activeBottomFrame win =
    activeBottomFrameHelper (LeftChild :: win.path) win
        |> Maybe.withDefault win


activeTopFrame : Window a -> Window a
activeTopFrame win =
    activeTopFrameHelper (LeftChild :: win.path) win
        |> Maybe.withDefault win



--logWindow : String -> (a -> String) -> Window a -> Window a
--logWindow s f win =
--    let
--        _ =
--            Debug.log s (toString f win)
--        _ =
--            toList win
--                |> List.filter .isActive
--                |> Debug.log s
--    in
--        win


activeRightFrameHelper : Path -> Window a -> Maybe (Window a)
activeRightFrameHelper path ({ current } as win) =
    case path of
        [] ->
            Nothing

        dir :: rest ->
            case current of
                Node (VSplit _) _ _ ->
                    case dir of
                        LeftChild ->
                            win
                                |> goRight
                                |> Maybe.map goLeftMost

                        RightChild ->
                            win
                                |> goParent
                                |> Maybe.andThen (activeRightFrameHelper rest)

                Node _ _ _ ->
                    win
                        |> goParent
                        |> Maybe.andThen (activeRightFrameHelper rest)

                _ ->
                    Nothing


activeLeftFrameHelper : Path -> Window a -> Maybe (Window a)
activeLeftFrameHelper path ({ current } as win) =
    case path of
        [] ->
            Nothing

        dir :: rest ->
            case current of
                Node (VSplit _) _ _ ->
                    case dir of
                        LeftChild ->
                            win
                                |> goParent
                                |> Maybe.andThen (activeLeftFrameHelper rest)

                        RightChild ->
                            win
                                |> goLeft
                                |> Maybe.map goLeftMost

                Node _ _ _ ->
                    win
                        |> goParent
                        |> Maybe.andThen (activeLeftFrameHelper rest)

                _ ->
                    Nothing


activeBottomFrameHelper : Path -> Window a -> Maybe (Window a)
activeBottomFrameHelper path ({ current } as win) =
    case path of
        [] ->
            Nothing

        dir :: rest ->
            case current of
                Node (HSplit _) _ _ ->
                    case dir of
                        LeftChild ->
                            win
                                |> goRight
                                |> Maybe.map goLeftMost

                        RightChild ->
                            win
                                |> goParent
                                |> Maybe.andThen (activeBottomFrameHelper rest)

                Node _ _ _ ->
                    win
                        |> goParent
                        |> Maybe.andThen (activeBottomFrameHelper rest)

                _ ->
                    Nothing


activeTopFrameHelper : Path -> Window a -> Maybe (Window a)
activeTopFrameHelper path ({ current } as win) =
    case path of
        [] ->
            Nothing

        dir :: rest ->
            case current of
                Node (HSplit _) _ _ ->
                    case dir of
                        LeftChild ->
                            win
                                |> goParent
                                |> Maybe.andThen (activeTopFrameHelper rest)

                        RightChild ->
                            win
                                |> goLeft
                                |> Maybe.map goLeftMost

                Node _ _ _ ->
                    win
                        |> goParent
                        |> Maybe.andThen (activeTopFrameHelper rest)

                _ ->
                    Nothing


goLeftMost : Zipper a -> Zipper a
goLeftMost ({ current } as zipper) =
    case current of
        Node _ Empty _ ->
            zipper

        Node _ _ _ ->
            case goLeft zipper of
                Just z ->
                    goLeftMost z

                _ ->
                    zipper

        _ ->
            zipper


goRightMost : Zipper a -> Zipper a
goRightMost ({ current } as zipper) =
    case current of
        Node _ _ Empty ->
            zipper

        Node _ _ _ ->
            case goRight zipper of
                Just z ->
                    goRightMost z

                _ ->
                    zipper

        _ ->
            zipper


setActive : (a -> Bool) -> Window a -> Window a
setActive pred win =
    win
        |> goRoot
        |> goToFrame pred


updateActiveFrame : (a -> a) -> Window a -> Window a
updateActiveFrame fn =
    updateSubtree
        (\tree1 ->
            case tree1 of
                Node (NoSplit frame) left right ->
                    Node (NoSplit (fn frame)) left right

                _ ->
                    tree1
        )


getFrame : Path -> Window a -> Maybe a
getFrame path win =
    case subtree (List.reverse path) win.tree of
        Just (Node (NoSplit frame) left right) ->
            Just frame

        _ ->
            Nothing


updateFrame : Path -> (a -> a) -> Window a -> Window a
updateFrame path fn win =
    if path == win.path then
        updateActiveFrame fn win

    else
        { win
            | tree =
                updateSubtreeHelper
                    (\tree1 ->
                        case tree1 of
                            Node (NoSplit frame) left right ->
                                Node (NoSplit (fn frame)) left right

                            _ ->
                                tree1
                    )
                    (List.reverse path)
                    win.tree
        }


mapFrame : (a -> ( Float, Float ) -> a) -> Window a -> Window a
mapFrame fn ({ tree } as win) =
    let
        tree1 =
            updateAllFramesHelper fn ( 1.0, 1.0 ) tree
    in
    { win
        | tree = tree1
        , current =
            tree1
                |> subtree (List.reverse win.path)
                |> Maybe.withDefault Empty
    }


updateAllFramesHelper :
    (a -> ( Float, Float ) -> a)
    -> ( Float, Float )
    -> Tree (WindowSplit a)
    -> Tree (WindowSplit a)
updateAllFramesHelper fn size tree =
    case tree of
        Node sp left right ->
            case sp of
                VSplit percent ->
                    let
                        leftSize =
                            Tuple.mapFirst ((*) percent) size

                        rightSize =
                            Tuple.mapFirst ((*) (1 - percent)) size
                    in
                    Node sp
                        (updateAllFramesHelper fn leftSize left)
                        (updateAllFramesHelper fn rightSize right)

                HSplit percent ->
                    let
                        topSize =
                            Tuple.mapSecond ((*) percent) size

                        bottomSize =
                            Tuple.mapSecond ((*) (1 - percent)) size
                    in
                    Node sp
                        (updateAllFramesHelper fn topSize left)
                        (updateAllFramesHelper fn bottomSize right)

                NoSplit frame ->
                    Node (NoSplit (fn frame size)) left right

        Empty ->
            Empty


setPercent : Float -> WindowSplit a -> WindowSplit a
setPercent percent sp =
    case sp of
        VSplit _ ->
            VSplit percent

        HSplit _ ->
            HSplit percent

        _ ->
            sp


setSize : Float -> Window a -> Window a
setSize percent win =
    let
        activeNode =
            Node (NoSplit <| getActiveFrame win) Empty Empty

        ( goChild, percent_ ) =
            case win.path of
                LeftChild :: _ ->
                    ( goLeft, percent )

                _ ->
                    ( goRight, 1 - percent )
    in
    win
        |> goParent
        |> Maybe.map
            (updateSubtree
                (\tree1 ->
                    case tree1 of
                        Node sp left right ->
                            Node (setPercent percent_ sp) left right

                        Empty ->
                            tree1
                )
            )
        |> Maybe.andThen goChild
        |> Maybe.withDefault win


{-| For unit test and debugging
-}
toString : (a -> String) -> Window a -> String
toString toStr { tree, path } =
    treeToString toStr tree path [] 0 Nothing


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


toBorders : Window a -> List Rect
toBorders win =
    toBordersHelper win.tree
        { y = 0, x = 0, width = 1.0, height = 1.0 }


toBordersHelper :
    Tree (WindowSplit a)
    -> Rect
    -> List Rect
toBordersHelper tree rect =
    case tree of
        Node (NoSplit id) Empty Empty ->
            []

        Node (VSplit percent) left right ->
            { x = rect.x + rect.width * percent
            , y = rect.y
            , height = rect.height
            , width = 0
            }
                :: toBordersHelper left
                    { rect
                        | width = rect.width * percent
                        , x = rect.x * percent
                    }
                ++ toBordersHelper right
                    { rect
                        | width = rect.width * (1 - percent)
                        , x = percent + rect.x * percent
                    }

        Node (HSplit percent) left right ->
            { x = rect.x
            , y = rect.y + rect.height * percent
            , width = rect.width
            , height = 0
            }
                :: toBordersHelper left
                    { rect
                        | height = rect.height * percent
                        , y = rect.y * percent
                    }
                ++ toBordersHelper right
                    { rect
                        | height = rect.height * (1 - percent)
                        , y = percent + rect.y * percent
                    }

        _ ->
            []


toList :
    Window a
    -> List { frame : a, rect : Rect, isActive : Bool, path : Path }
toList win =
    toListHelper win.tree win.path []


toListHelper :
    Tree (WindowSplit a)
    -> Path
    -> Path
    -> List { frame : a, rect : Rect, isActive : Bool, path : Path }
toListHelper tree activeFramePath path =
    let
        updateRect fn res =
            { res | rect = fn res.rect }
    in
    case tree of
        Node (NoSplit id) Empty Empty ->
            [ { frame = id
              , rect = { x = 0, y = 0, width = 1.0, height = 1.0 }
              , isActive = activeFramePath == path
              , path = path
              }
            ]

        Node (VSplit percent) left right ->
            (toListHelper left activeFramePath (LeftChild :: path)
                |> List.map
                    (updateRect
                        (\rect ->
                            { rect
                                | width = rect.width * percent
                                , x = rect.x * percent
                            }
                        )
                    )
            )
                ++ (toListHelper right activeFramePath (RightChild :: path)
                        |> List.map
                            (updateRect
                                (\rect ->
                                    { rect
                                        | width = rect.width * (1 - percent)
                                        , x = percent + rect.x * percent
                                    }
                                )
                            )
                   )

        Node (HSplit percent) left right ->
            (toListHelper left activeFramePath (LeftChild :: path)
                |> List.map
                    (updateRect
                        (\rect ->
                            { rect
                                | height = rect.height * percent
                                , y = rect.y * percent
                            }
                        )
                    )
            )
                ++ (toListHelper right activeFramePath (RightChild :: path)
                        |> List.map
                            (updateRect
                                (\rect ->
                                    { rect
                                        | height = rect.height * (1 - percent)
                                        , y = percent + rect.y * percent
                                    }
                                )
                            )
                   )

        _ ->
            []


treeToString :
    (a -> String)
    -> Tree (WindowSplit a)
    -> Path
    -> Path
    -> Int
    -> Maybe (Tree (WindowSplit a))
    -> String
treeToString toStr tree dirs currentDirs deep parent =
    case tree of
        Node (NoSplit frame) Empty Empty ->
            (if dirs == currentDirs then
                "@"

             else
                ""
            )
                ++ toStr frame
                ++ "    "
                ++ String.fromInt
                    (let
                        isRightChild =
                            case currentDirs of
                                RightChild :: _ ->
                                    True

                                _ ->
                                    False

                        percent =
                            case parent of
                                Just (Node (VSplit f) _ _) ->
                                    f

                                Just (Node (HSplit f) _ _) ->
                                    f

                                _ ->
                                    1.0
                     in
                     (if isRightChild then
                        1 - percent

                      else
                        percent
                     )
                        * 100
                        |> floor
                    )
                ++ "%"

        Node d left right ->
            let
                spDir =
                    case d of
                        VSplit f ->
                            "v────"

                        HSplit f ->
                            "h────"

                        _ ->
                            ""

                indent =
                    String.repeat deep "│    "
            in
            (spDir
                ++ treeToString toStr
                    left
                    dirs
                    (LeftChild :: currentDirs)
                    (deep + 1)
                    (Just tree)
            )
                ++ "\n"
                ++ (indent
                        ++ "└────"
                        ++ treeToString toStr
                            right
                            dirs
                            (RightChild :: currentDirs)
                            (deep + 1)
                            (Just tree)
                   )

        _ ->
            ""


treeDecoder : Decoder a -> Decoder (Tree a)
treeDecoder decoder =
    Decode.oneOf
        [ Decode.null Empty
        , Decode.lazy
            (\_ ->
                Decode.map3 (\a left right -> Node a left right)
                    (Decode.field "data" decoder)
                    (Decode.field "left" <| treeDecoder decoder)
                    (Decode.field "right" <| treeDecoder decoder)
            )
        ]


windowSplitDecoder : Decoder a -> Decoder (WindowSplit a)
windowSplitDecoder frameDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\tp ->
                case tp of
                    "vsplit" ->
                        Decode.field "percent" Decode.float
                            |> Decode.map VSplit

                    "hsplit" ->
                        Decode.field "percent" Decode.float
                            |> Decode.map HSplit

                    "nosplit" ->
                        Decode.field "frame" frameDecoder
                            |> Decode.map NoSplit

                    _ ->
                        Decode.fail ("invalid split type: " ++ tp)
            )


pathDecoder : Decoder Path
pathDecoder =
    Decode.string
        |> Decode.andThen
            (\dir ->
                case dir of
                    "l" ->
                        Decode.succeed LeftChild

                    "r" ->
                        Decode.succeed RightChild

                    _ ->
                        Decode.fail ("invalid direction: " ++ dir)
            )
        |> Decode.list


windowDecoder : Decoder a -> Decoder (Window a)
windowDecoder frameDecoder =
    Decode.map2
        (\tree path ->
            { tree = tree
            , path = path
            , current =
                subtree (List.reverse path) tree
                    |> Maybe.withDefault Empty
            }
        )
        (Decode.field "tree" <| treeDecoder (windowSplitDecoder frameDecoder))
        (Decode.field "path" pathDecoder)


windowSplitEncoder : (a -> Encode.Value) -> WindowSplit a -> Encode.Value
windowSplitEncoder frameEncoder sp =
    case sp of
        NoSplit a ->
            Encode.object
                [ ( "type", Encode.string "nosplit" )
                , ( "frame", frameEncoder a )
                ]

        VSplit percent ->
            Encode.object
                [ ( "type", Encode.string "vsplit" )
                , ( "percent", Encode.float percent )
                ]

        HSplit percent ->
            Encode.object
                [ ( "type", Encode.string "hsplit" )
                , ( "percent", Encode.float percent )
                ]


treeEncoder : (a -> Encode.Value) -> Tree (WindowSplit a) -> Encode.Value
treeEncoder encoder tree =
    case tree of
        Node a left right ->
            Encode.object
                [ ( "data", windowSplitEncoder encoder a )
                , ( "left", treeEncoder encoder left )
                , ( "right", treeEncoder encoder right )
                ]

        _ ->
            Encode.null


pathEncoder : Path -> Encode.Value
pathEncoder =
    Encode.list
        (\dir ->
            case dir of
                LeftChild ->
                    Encode.string "l"

                RightChild ->
                    Encode.string "r"
        )


windowEncoder : (a -> Encode.Value) -> Window a -> Encode.Value
windowEncoder frameEncoder win =
    Encode.object
        [ ( "tree", treeEncoder frameEncoder win.tree )
        , ( "path", pathEncoder win.path )
        ]
