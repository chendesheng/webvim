module TestSyntax exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Internal.Syntax exposing (..)
import Elm.Array as Array exposing (Array)
import Internal.TextBuffer as B exposing (Patch(..))
import Helper.Helper exposing (..)


testSyntax : Syntax
testSyntax =
    Array.fromList
        [ [ { length = 2, classname = "mtk1", tipe = TokenOther } ]
        , [ { length = 2, classname = "mtk2", tipe = TokenOther } ]
        ]


emptySyntax : Syntax
emptySyntax =
    Array.empty


newToken2 : Int -> String -> Token
newToken2 length classname =
    { length = length
    , classname = classname
    , tipe = TokenOther
    }


newToken : Int -> Token
newToken length =
    { length = length
    , classname = ""
    , tipe = TokenOther
    }


cases :
    List
        { description : String
        , patches : List Patch
        , result : ( List (List Token), Maybe Int )
        , syntax : List (List Token)
        }
cases =
    [ { description = "insert into empty syntax"
      , syntax = []
      , patches = [ Insertion ( 0, 0 ) (B.fromString "aa\n") ]
      , result =
            ( [ [ newToken 3 ]
              , [ newToken 0 ]
              ]
            , Just 0
            )
      }
    , { description = "insert to left"
      , syntax = [ [ newToken2 3 "mtk1" ] ]
      , patches = [ Insertion ( 0, 0 ) (B.fromString "aa") ]
      , result =
            ( [ [ newToken2 2 "mtk1"
                , newToken2 3 "mtk1"
                ]
              ]
            , Just 0
            )
      }
    , { description = "insert to right"
      , syntax = [ [ newToken2 3 "mtk2" ] ]
      , patches = [ Insertion ( 0, 3 ) (B.fromString "aa") ]
      , result =
            ( [ [ newToken2 5 "mtk2" ]
              ]
            , Just 0
            )
      }
    , { description = "insert to middle"
      , syntax = [ [ newToken2 3 "mtk1" ] ]
      , patches = [ Insertion ( 0, 2 ) (B.fromString "aa") ]
      , result =
            ( [ [ newToken2 4 "mtk1"
                , newToken2 1 "mtk1"
                ]
              ]
            , Just 0
            )
      }
    , { description = "insert to tokens middle"
      , syntax =
            [ [ newToken2 3 "mtk1"
              , newToken2 4 "mtk2"
              ]
            ]
      , patches = [ Insertion ( 0, 3 ) (B.fromString "aa") ]
      , result =
            ( [ [ newToken2 5 "mtk1"
                , newToken2 4 "mtk2"
                ]
              ]
            , Just 0
            )
      }
    , { description = "insert to more tokens middle"
      , syntax =
            [ [ newToken2 3 "mtk1"
              , newToken2 4 "mtk2"
              , newToken2 4 "mtk3"
              ]
            ]
      , patches = [ Insertion ( 0, 5 ) (B.fromString "aa") ]
      , result =
            ( [ [ newToken2 3 "mtk1"
                , newToken2 4 "mtk2"
                , newToken2 2 "mtk2"
                , newToken2 4 "mtk3"
                ]
              ]
            , Just 0
            )
      }
    , { description = "insert to top"
      , syntax =
            [ [ newToken2 3 "mtk1" ]
            , [ newToken2 3 "mtk2" ]
            ]
      , patches = [ Insertion ( 0, 0 ) (B.fromString "a\n") ]
      , result =
            ( [ [ newToken2 2 "mtk1" ]
              , [ newToken2 3 "mtk1" ]
              , [ newToken2 3 "mtk2" ]
              ]
            , Just 0
            )
      }
    , { description = "insert to lines middle"
      , syntax =
            [ [ newToken2 3 "mtk1" ]
            , [ newToken2 3 "mtk2" ]
            ]
      , patches = [ Insertion ( 1, 0 ) (B.fromString "a\n") ]
      , result =
            ( [ [ newToken2 3 "mtk1" ]
              , [ newToken2 2 "mtk2" ]
              , [ newToken2 3 "mtk2" ]
              ]
            , Just 1
            )
      }
    , { description = "delete line"
      , syntax =
            [ [ newToken2 3 "mtk1" ]
            , [ newToken2 5 "mtk2" ]
            ]
      , patches = [ Deletion ( 0, 0 ) ( 1, 0 ) ]
      , result =
            ( [ [ newToken2 5 "mtk2" ] ]
            , Just 0
            )
      }
    , { description = "delete lines"
      , syntax =
            [ [ newToken2 3 "mtk1" ]
            , [ newToken2 5 "mtk2" ]
            , [ newToken2 6 "mtk3" ]
            ]
      , patches = [ Deletion ( 0, 0 ) ( 2, 3 ) ]
      , result =
            ( [ [ newToken2 3 "mtk3" ] ]
            , Just 0
            )
      }
    , { description = "delete char"
      , syntax =
            [ [ newToken2 6 "mtk7"
              , newToken2 1 "mtk1"
              , newToken2 6 "mtk8"
              , newToken2 2 "mtk1"
              ]
            ]
      , patches = [ Deletion ( 0, 7 ) ( 0, 8 ) ]
      , result =
            ( [ [ newToken2 6 "mtk7"
                , newToken2 1 "mtk1"
                , newToken2 5 "mtk8"
                , newToken2 2 "mtk1"
                ]
              ]
            , Just 0
            )
      }
    , { description = "insert to bottom"
      , syntax =
            [ [ newToken2 3 "mtk1" ]
            , [ newToken2 3 "mtk2" ]
            ]
      , patches = [ Insertion ( 2, 0 ) (B.fromString "a") ]
      , result =
            ( [ [ newToken2 3 "mtk1" ]
              , [ newToken2 3 "mtk2" ]
              , [ newToken2 1 "" ]
              ]
            , Just 2
            )
      }
    ]


suite : Test
suite =
    describe "Patch tokens"
        (List.map
            (\item ->
                test item.description <|
                    \_ ->
                        let
                            ( newSyntax, n ) =
                                List.foldl
                                    (\patch result ->
                                        let
                                            ( syntax, n ) =
                                                result

                                            ( newSyntax, newn ) =
                                                applyPatchToSyntax patch syntax
                                        in
                                            ( newSyntax, minMaybe n newn )
                                    )
                                    ( Array.fromList item.syntax, Nothing )
                                    item.patches
                        in
                            Expect.equal
                                item.result
                                ( Array.toList newSyntax, n )
            )
            --(List.drop (List.length cases - 1) cases)
            cases
        )
