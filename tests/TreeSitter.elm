module TreeSitter
    exposing
    -- This is a fack module, just for elm-test
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


type alias Input =
    InputCallbackArgument -> String


createParser : String -> Maybe Parser
createParser _ =
    Nothing


parse : Parser -> Input -> Tree
parse _ _ =
    Tree


incParse : Parser -> List Edit -> Input -> Tree -> ( Tree, List Range )
incParse _ _ _ _ =
    ( Tree, [] )


dummyParser : Parser
dummyParser =
    Parser



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
root _ =
    Node


childCount : Node -> Int
childCount _ =
    0


child : Int -> Node -> Maybe Node
child _ _ =
    Nothing


children : Node -> List Node
children _ =
    []


getScope : Node -> NodeScope
getScope _ =
    { scope = ""
    , startIndex = 0
    , endIndex = 0
    , startPosition = ( 0, 0 )
    , endPosition = ( 0, 0 )
    }


walkWithin :
    ( Int, Int )
    -> ( Int, Int )
    -> (List NodeScope -> state -> state)
    -> Tree
    -> state
    -> state
walkWithin _ _ _ _ state =
    state
