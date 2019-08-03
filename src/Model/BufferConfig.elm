module Model.BufferConfig exposing (BufferConfig, IndentConfig(..), cIndentRules, configs, cssFileDefaultConfig, defaultBufferConfig)

import Dict exposing (Dict)
import Helper.Helper
    exposing
        ( extname
        , filename
        , findFirst
        , rangeCount
        , regex
        , relativePath
        )
import Regex as Re


type IndentConfig
    = AutoIndent -- same indent as last line
    | IndentRules
        { increase : Re.Regex
        , increaseNext : Re.Regex
        , decrease : Re.Regex
        , trigger : String
        }


type alias BufferConfig =
    { wordChars : String -- a-z and A-Z are word chars by default
    , tabSize : Int
    , expandTab : Bool
    , lint : Bool
    , indent : IndentConfig
    , syntax : Bool
    }


defaultBufferConfig : BufferConfig
defaultBufferConfig =
    { wordChars = "_"
    , tabSize = 2
    , expandTab = True
    , lint = False
    , indent = AutoIndent
    , syntax = False
    }


cIndentRules :
    { decrease : Re.Regex
    , increase : Re.Regex
    , increaseNext : Re.Regex
    , trigger : String
    }
cIndentRules =
    { increase = regex "^.*\\{[^}\\\"']*$"
    , decrease = regex "^(.*\\*/)?\\s*\\}[;\\s]*$"
    , increaseNext =
        regex "^(?!.*;\\s*//).*[^\\s;{}]\\s*$"
    , trigger = "}"
    }


cssFileDefaultConfig : BufferConfig
cssFileDefaultConfig =
    { defaultBufferConfig
        | tabSize = 2
        , indent = IndentRules cIndentRules
        , wordChars = "_-.#"
    }


configs : Dict String BufferConfig
configs =
    Dict.fromList
        [ ( ".elm"
          , { defaultBufferConfig
                | tabSize = 4
                , lint = True
                , indent =
                    IndentRules
                        { increase =
                            regex
                                ("(^[(]?let$)|(^[(]?if)"
                                    ++ "|(^then$)|(^else(\\s|$))|(=$)"
                                    ++ "|(^in$)|(^[(]?case)|(^of$)|(->$)"
                                )
                        , decrease = regex "^(then|else( if)?|of|in)"
                        , increaseNext = regex "![\\s\\S]"
                        , trigger = ""
                        }
            }
          )
        , ( ".js"
          , { defaultBufferConfig
                | tabSize = 2
                , lint = True
                , indent = IndentRules cIndentRules
            }
          )
        , ( ".jsx"
          , { defaultBufferConfig
                | tabSize = 2
                , lint = True
                , indent = IndentRules cIndentRules
            }
          )
        , ( ".purs"
          , { defaultBufferConfig
                | tabSize = 2
                , indent =
                    IndentRules
                        { increase =
                            regex
                                ("(^[(]?let$)|(^[(]?if)"
                                    ++ "|(^then$)|(^else(\\s|$))|(=$)"
                                    ++ "|(^in$)|(^[(]?case)|(^of$)|(->$)"
                                    ++ "|(^when)|(\\sdo$)"
                                )
                        , decrease = regex "^(then|else( if)?|of|in)"
                        , increaseNext = regex "![\\s\\S]"
                        , trigger = ""
                        }
            }
          )
        , ( ".less", cssFileDefaultConfig )
        , ( ".css", cssFileDefaultConfig )
        ]
