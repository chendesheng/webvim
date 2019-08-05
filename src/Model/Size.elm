module Model.Size exposing (Size, emptySize)


type alias Size =
    { width : Int
    , height : Int
    }


emptySize : Size
emptySize =
    { width = 0, height = 0 }
