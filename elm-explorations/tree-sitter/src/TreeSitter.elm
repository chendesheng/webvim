module TreeSitter exposing
    ( Edit
    , Input
    , InputCallbackArgument
    , Node
    , NodeScope
    , Parser
    , Tree
    , child
    , childCount
    , children
    , createParser
    , dummyParser
    , getScope
    , incParse
    , parse
    , root
    , walkWithin
    )

import Elm.Kernel.TreeSitter



-- PARSER


type Parser
    = Parser


type alias Point =
    ( Int, Int )


type alias Range =
    { startPosition : Point
    , endPosition : Point
    }


type alias Edit =
    { startIndex : Int
    , newEndIndex : Int
    , oldEndIndex : Int
    , startPoint : Point
    , newEndPoint : Point
    , oldEndPoint : Point
    }


type alias InputCallbackArgument =
    { startIndex : Int
    , startPoint : Maybe Point
    , endIndex : Maybe Int
    }


type alias Input text =
    text -> InputCallbackArgument -> ( String, text )


createParser : String -> Maybe Parser
createParser =
    Elm.Kernel.TreeSitter.createParser


parse : Parser -> Input text -> text -> Tree
parse =
    Elm.Kernel.TreeSitter.parse


incParse : Parser -> List Edit -> Input text -> Tree -> ( Tree, List Range )
incParse =
    Elm.Kernel.TreeSitter.incParse


dummyParser : Parser
dummyParser =
    Elm.Kernel.TreeSitter.dummyParser



-- TREE


type Tree
    = Tree


type Node
    = Node


type alias NodeScope =
    { scope : String
    , startIndex : Int
    , endIndex : Int
    , startPosition : ( Int, Int )
    , endPosition : ( Int, Int )
    }


root : Tree -> Node
root =
    Elm.Kernel.TreeSitter.root


childCount : Node -> Int
childCount =
    Elm.Kernel.TreeSitter.childCount


child : Int -> Node -> Maybe Node
child =
    Elm.Kernel.TreeSitter.child


children : Node -> List Node
children =
    Elm.Kernel.TreeSitter.children


getScope : Node -> NodeScope
getScope =
    Elm.Kernel.TreeSitter.getScope


walkWithin :
    ( Int, Int )
    -> ( Int, Int )
    -> (List NodeScope -> state -> state)
    -> Tree
    -> state
    -> state
walkWithin =
    Elm.Kernel.TreeSitter.walkWithin
