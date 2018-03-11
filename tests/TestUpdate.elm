module TestUpdate exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Update exposing (update)
import Model exposing (..)
import Internal.TextBuffer as B exposing (Patch(..))
import Dict
import Message exposing (Msg(..), Key)
import Parser as P exposing ((|.), (|=), Parser)


buffer2Model : Buffer -> Model
buffer2Model buf =
    { buffers = Dict.fromList [ ( buf.id, buf ) ]
    , maxId = 0
    , view = buf.view
    }


testModel : Model
testModel =
    buffer2Model emptyBuffer


handleKeys : List Key -> Model -> Model
handleKeys keys model =
    List.foldl
        (\msg model ->
            update msg model
                |> Tuple.first
        )
        model
        (List.map (PressKey 0) keys)


keysParser : Parser (List String)
keysParser =
    P.keep (P.Exactly 1) (always True)
        |> P.andThen
            (\s ->
                case s of
                    "\\" ->
                        P.keep (P.Exactly 1) (\c -> c == '\\' || c == '<')

                    "<" ->
                        P.ignoreUntil ">"
                            |> P.source
                            |> P.map ((++) "<")

                    _ ->
                        P.succeed s
            )
        |> P.repeat P.zeroOrMore


insertCases : List ( String, Buffer )
insertCases =
    [ ( "i"
      , { emptyBuffer
            | mode = Insert
            , continuation = "i"
        }
      )
    , ( "i12"
      , { emptyBuffer
            | cursor = ( 0, 2 )
            , lines = B.fromList [ "12" ]
            , mode = Insert
            , continuation = "i"
            , history =
                { emptyBufferHistory
                    | pending =
                        Just
                            { cursor = ( 0, 0 )
                            , patches =
                                [ Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                            }
                }
        }
      )
    , ( "i12<esc>"
      , { emptyBuffer
            | cursor = ( 0, 1 )
            , lines = B.fromList [ "12" ]
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                          }
                        ]
                }
        }
      )
    , ( "i12<cr><esc>"
      , { emptyBuffer
            | cursor = ( 1, 0 )
            , lines = B.fromList [ "12\n", "" ]
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 2 ) ( 1, 0 )
                                , Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                          }
                        ]
                }
        }
      )
    , ( "i12<tab><tab><esc>"
      , { emptyBuffer
            | cursor = ( 0, 7 )
            , lines = B.fromList [ "12      " ]
            , history =
                { emptyBufferHistory
                    | undoes =
                        [ { cursor = ( 0, 0 )
                          , patches =
                                [ Deletion ( 0, 4 ) ( 0, 8 )
                                , Deletion ( 0, 2 ) ( 0, 4 )
                                , Deletion ( 0, 1 ) ( 0, 2 )
                                , Deletion ( 0, 0 ) ( 0, 1 )
                                ]
                          }
                        ]
                }
        }
      )
    ]


suite : Test
suite =
    describe "Press keys"
        [ describe "insert cases"
            (List.map
                (\( s, buf ) ->
                    s
                        |> P.run keysParser
                        |> Result.map
                            (\keys ->
                                let
                                    buf1 =
                                        handleKeys keys testModel
                                            |> .buffers
                                            |> Dict.get 0
                                            |> Maybe.withDefault emptyBuffer
                                in
                                    test s <|
                                        \_ ->
                                            Expect.equal buf buf1
                            )
                        |> Result.withDefault
                            (test s <| \_ -> Expect.fail "invalid input")
                )
                insertCases
            )
        ]
