module Syntax
    exposing
        ( Syntax
        , SyntaxLine
        , Scope
        , highlight
        , Continuation
        )

import Array exposing (Array)
import Native.Highlight


type alias Scope =
    ( String, String )


type alias SyntaxLine =
    ( List Scope, Continuation )


type alias Syntax =
    { lang : String
    , lines : Array SyntaxLine
    }


replace : Int -> Int -> List SyntaxLine -> Syntax -> Syntax
replace a b lines syn =
    let
        conti =
            Array.get (a - 1) syn.lines
                |> Maybe.map Tuple.second
    in
        { lines =
            List.foldl
                Array.append
                Array.empty
                [ (Array.slice 0 a syn.lines)
                , Array.fromList lines
                , (Array.slice b (Array.length syn.lines) syn.lines)
                ]
        , lang = syn.lang
        }


type Continuation
    = Continuation


highlight :
    String
    -> String
    -> Maybe Continuation
    -> SyntaxLine
highlight =
    Native.Highlight.highlight



--highlight lang lines =
--        List.foldl
--            (\line result ->
--                let
--                    sline =
--                        highlight
--                            lang
--                            line
--                            conti
--                in
--                    Array.append result <|
--                        Array.fromList [ sline ]
--            )
--            Array.empty
--            lines
