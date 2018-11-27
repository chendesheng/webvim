module Internal.Window
    exposing
        ( Window
        , WindowRect
        , WindowSplit(..)
        , getActiveView
        , activeNextView
        , setSize
        , setActive
        , vsplit
        , hsplit
        , toString
        , empty
        , initWindow
        , toList
        , removeCurrent
        , updateView
        )

import Vim.Helper exposing (dropLast)
import Helper.Helper exposing (dropWhile)


-- Tree


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


type Direction
    = LeftChild
    | RightChild


type alias Zipper a =
    { tree : Tree a
    , current : Tree a
    , dirs : List Direction
    }


{-| return subtree and path to the subtree
-}
searchTree :
    Bool
    -> (a -> Bool)
    -> Tree a
    -> List Direction
    -> Maybe ( Tree a, List Direction )
searchTree includeCurrent pred tree dirs =
    case tree of
        Node a left right ->
            if pred a && includeCurrent then
                Just ( tree, dirs )
            else
                case searchTree True pred left (LeftChild :: dirs) of
                    Nothing ->
                        searchTree True pred right (RightChild :: dirs)

                    res ->
                        res

        _ ->
            Nothing


{-| find start from current position (include current position)
-}
find : Bool -> (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find includeCurrent pred ({ tree, current, dirs } as z) =
    case current of
        Node a _ right ->
            case searchTree includeCurrent pred current [] of
                Nothing ->
                    case
                        dirs
                            |> dropWhile ((==) RightChild)
                            |> List.tail
                            |> Maybe.andThen
                                (\dirs1 ->
                                    subtree (List.reverse dirs1) tree
                                        |> Maybe.map (Tuple.pair dirs1)
                                )
                    of
                        Just ( dirs1, tree1 ) ->
                            case tree1 of
                                Node _ _ right1 ->
                                    find True
                                        pred
                                        { tree = tree
                                        , current = right1
                                        , dirs = RightChild :: dirs1
                                        }

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                Just ( tree1, dirs1 ) ->
                    Just
                        { tree = tree
                        , current = tree1
                        , dirs = dirs1 ++ dirs
                        }

        Empty ->
            Nothing


findHelper : List Direction -> (a -> Bool) -> Tree a -> Maybe ( Tree a, List Direction )
findHelper path pred tree =
    case tree of
        Node a left right ->
            if pred a then
                Just ( tree, path )
            else
                case findHelper (LeftChild :: path) pred left of
                    Nothing ->
                        findHelper (RightChild :: path) pred right

                    p ->
                        p

        Empty ->
            Nothing


subtree : List Direction -> Tree a -> Maybe (Tree a)
subtree path tree =
    case path of
        dir :: rest ->
            case dir of
                LeftChild ->
                    case tree of
                        Node _ left _ ->
                            subtree rest left

                        _ ->
                            Nothing

                RightChild ->
                    case tree of
                        Node _ _ right ->
                            subtree rest right

                        _ ->
                            Nothing

        _ ->
            Just tree


updateSubtree : (Tree a -> Tree a) -> Zipper a -> Zipper a
updateSubtree fn { tree, current, dirs } =
    { tree = updateSubtreeHelper fn (List.reverse dirs) tree
    , dirs = dirs
    , current = fn current
    }


updateSubtreeHelper : (Tree a -> Tree a) -> List Direction -> Tree a -> Tree a
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
goRoot { tree, current, dirs } =
    { tree = tree
    , current = tree
    , dirs = []
    }


goLeft : Zipper a -> Maybe (Zipper a)
goLeft { tree, dirs, current } =
    case current of
        Node _ left _ ->
            Just
                { tree = tree
                , dirs = LeftChild :: dirs
                , current = left
                }

        _ ->
            Nothing


goParent : Zipper a -> Maybe (Zipper a)
goParent { tree, dirs, current } =
    case dirs of
        [] ->
            Nothing

        _ :: ancests ->
            tree
                |> subtree (List.reverse ancests)
                |> Maybe.map
                    (\tree1 ->
                        { tree = tree
                        , current = tree1
                        , dirs = ancests
                        }
                    )


goRight : Zipper a -> Maybe (Zipper a)
goRight { tree, dirs, current } =
    case current of
        Node _ _ right ->
            Just
                { tree = tree
                , dirs = RightChild :: dirs
                , current = right
                }

        _ ->
            Nothing


isRoot : Zipper a -> Bool
isRoot { dirs } =
    List.isEmpty dirs



-- Window


type WindowSplit a
    = HSplit Float
    | VSplit Float
    | NoSplit a


type alias Window a =
    Zipper (WindowSplit a)


empty : Window a
empty =
    { tree = Empty, current = Empty, dirs = [] }


initWindow : a -> Window a
initWindow view =
    let
        tree =
            Node (NoSplit view) Empty Empty
    in
        { tree = tree
        , current = tree
        , dirs = []
        }


getActiveView : Window a -> Maybe a
getActiveView { current } =
    case current of
        Node (NoSplit v) _ _ ->
            Just v

        _ ->
            Nothing


goToView : (a -> Bool) -> Window a -> Window a
goToView pred win =
    win
        |> find True
            (\node ->
                case node of
                    NoSplit view ->
                        pred view

                    _ ->
                        False
            )
        |> Maybe.withDefault win


vsplit : Float -> a -> Window a -> Window a
vsplit percent id win =
    split (VSplit (1 - (max percent 0.05))) id win


hsplit : Float -> a -> Window a -> Window a
hsplit percent id win =
    split (HSplit (1 - (max percent 0.05))) id win


split : WindowSplit a -> a -> Window a -> Window a
split sp view win =
    win
        |> updateSubtree
            (\tree ->
                case tree of
                    Node w _ _ ->
                        case w of
                            NoSplit id ->
                                Node sp
                                    tree
                                    (Node (NoSplit view)
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
                    case win.dirs of
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
                            >> activeNextViewHelper False
                        )
                    |> Maybe.withDefault win


activeNextView : Window a -> Window a
activeNextView win =
    activeNextViewHelper True win


activeNextViewHelper : Bool -> Window a -> Window a
activeNextViewHelper excludeCurrent win =
    let
        view =
            if excludeCurrent then
                getActiveView win
            else
                Nothing

        findView =
            find False
                (\nd ->
                    case nd of
                        NoSplit v ->
                            True

                        _ ->
                            False
                )
    in
        case findView win of
            Nothing ->
                if isRoot win then
                    -- do nothing if already root
                    win
                else
                    win
                        |> goRoot
                        |> findView
                        |> Maybe.withDefault win

            Just win1 ->
                win1


setActive : (a -> Bool) -> Window a -> Window a
setActive pred win =
    win
        |> goRoot
        |> goToView pred


updateView : (a -> a) -> Window a -> Window a
updateView fn =
    updateSubtree
        (\tree1 ->
            case tree1 of
                Node (NoSplit view) left right ->
                    Node (NoSplit (fn view)) left right

                _ ->
                    tree1
        )


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
            Node (NoSplit <| getActiveView win) Empty Empty

        ( goChild, percent_ ) =
            case win.dirs of
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
toString toStr { tree, dirs } =
    treeToString toStr tree dirs [] 0 Nothing


type alias WindowRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


toList : Window a -> List ( a, WindowRect )
toList win =
    toListHelper win.tree []


toListHelper : Tree (WindowSplit a) -> List ( a, WindowRect ) -> List ( a, WindowRect )
toListHelper tree result =
    case tree of
        Node (NoSplit id) Empty Empty ->
            [ ( id, { x = 0, y = 0, width = 1, height = 1 } ) ]

        Node (VSplit percent) left right ->
            (toListHelper left []
                |> List.map
                    (Tuple.mapSecond
                        (\rect ->
                            { rect
                                | width = rect.width * percent
                                , x = rect.x * percent
                            }
                        )
                    )
            )
                ++ (toListHelper right []
                        |> List.map
                            (Tuple.mapSecond
                                (\rect ->
                                    { rect
                                        | width = rect.width * (1 - percent)
                                        , x = percent + rect.x * percent
                                    }
                                )
                            )
                   )

        Node (HSplit percent) left right ->
            (toListHelper left []
                |> List.map
                    (Tuple.mapSecond
                        (\rect ->
                            { rect
                                | height = rect.height * percent
                                , y = rect.y * percent
                            }
                        )
                    )
            )
                ++ (toListHelper right []
                        |> List.map
                            (Tuple.mapSecond
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
    -> List Direction
    -> List Direction
    -> Int
    -> Maybe (Tree (WindowSplit a))
    -> String
treeToString toStr tree dirs currentDirs deep parent =
    case tree of
        Node (NoSplit view) Empty Empty ->
            (if dirs == currentDirs then
                "@"
             else
                ""
            )
                ++ toStr view
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
