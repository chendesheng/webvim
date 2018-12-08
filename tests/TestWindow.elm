module TestWindow exposing (..)

import Internal.Window as W
import Test exposing (..)
import Expect exposing (Expectation)


suite : Test
suite =
    describe "window"
        [ test "init"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
@1    100%
""")
            )
        , test "init v"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                            |> W.vsplit 0.3 2
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
v────@1    70%
└────2    30%
""")
            )
        , test "init h"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                            |> W.hsplit 0.5 2
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
h────@1    50%
└────2    50%
""")
            )
        , test "init nested"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                            |> W.hsplit 0.5 2
                            |> W.vsplit 0.5 3
                            |> W.vsplit 0.5 4
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
h────v────v────@1    50%
│    │    └────4    50%
│    └────3    50%
└────2    50%
""")
            )
        , test "setActive"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                            |> W.hsplit 0.5 2
                            |> W.vsplit 0.5 3
                            |> W.vsplit 0.5 4
                            |> W.setActive ((==) 3)
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
h────v────v────1    50%
│    │    └────4    50%
│    └────@3    50%
└────2    50%
""")
            )
        , test "activeNextWindow"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                            |> W.hsplit 0.5 2
                            |> W.vsplit 0.5 3
                            |> W.vsplit 0.5 4
                            |> W.activeNextView
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
h────v────v────1    50%
│    │    └────@4    50%
│    └────3    50%
└────2    50%
""")
            )
        , test "setSize"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                            |> W.hsplit 0.5 2
                            |> W.vsplit 0.4 3
                            |> W.vsplit 0.3 4
                            |> W.setActive ((==) 2)
                            |> W.setSize 0.3
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
h────v────v────1    70%
│    │    └────4    30%
│    └────3    40%
└────@2    30%
""")
            )
        , test "removeCurrent"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                            |> W.hsplit 0.5 2
                            |> W.vsplit 0.5 3
                            |> W.vsplit 0.5 4
                            |> W.setActive ((==) 3)
                            |> W.removeCurrent
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
h────v────@1    50%
│    └────4    50%
└────2    50%
""")
            )
        , test "removeCurrent back"
            (\_ ->
                let
                    window =
                        W.initWindow 1
                            |> W.hsplit 0.5 2
                            |> W.vsplit 0.5 3
                            |> W.vsplit 0.5 4
                            |> W.setActive ((==) 2)
                            |> W.removeCurrent
                in
                    Expect.equal (W.toString String.fromInt window)
                        (String.trim """
v────v────@1    50%
│    └────4    50%
└────3    50%
""")
            )
        , test "toList"
            (\_ ->
                let
                    list =
                        W.initWindow 1
                            |> W.hsplit 0.5 2
                            |> W.vsplit 0.5 3
                            |> W.vsplit 0.5 4
                            |> W.toList
                in
                    Expect.equal list
                        [ { view = 1
                          , rect =
                                { x = 0
                                , y = 0
                                , width = 0.25
                                , height = 0.5
                                }
                          , isActive = True
                          }
                        , { view = 4
                          , rect =
                                { x = 0.25
                                , y = 0
                                , width = 0.25
                                , height = 0.5
                                }
                          , isActive = False
                          }
                        , { view = 3
                          , rect =
                                { x = 0.5
                                , y = 0
                                , width = 0.5
                                , height = 0.5
                                }
                          , isActive = False
                          }
                        , { view = 2
                          , rect =
                                { x = 0
                                , y = 0.5
                                , width = 1
                                , height = 0.5
                                }
                          , isActive = False
                          }
                        ]
            )
        ]
