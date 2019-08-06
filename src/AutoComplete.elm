module AutoComplete exposing (Model, render)

import Helper.Fuzzy exposing (FuzzyMatchItem, fuzzyMatch)
import Helper.Helper
    exposing
        ( getLast
        , pathBase
        , replaceHomeDir
        , resolvePath
        )
import Html exposing (..)
import Internal.Position exposing (Position)
import Menu as Mu


type Model
    = Model
        { source : List String
        , menu : Mu.Model FuzzyMatchItem
        , word : String
        , wordChars : String
        , pos : Position

        -- if word changed, provider may also changed
        -- if provider changed then source list need re-populated
        , provider : AutoCompleteProvider
        , menuLeftOffset : Int
        }


type AutoCompleteProvider
    = FileName String -- including directory
    | DirectoryName String -- directory only
    | AllFiles String -- git ls files
    | WordsInBuffer Int -- buffer id
    | ExHistory
    | BufferNames


init word pos menuLeftOffset provider wordChars =
    ( { source = []
      , menu = Mu.init 10 []
      , menuLeftOffset = menuLeftOffset
      , provider = provider
      , word = word
      , wordChars = wordChars
      }
    , Cmd.none
    )



-- UPDATE


{-| return new model and current selected item
-}
selectForward : Model -> ( Model, String )
selectForward (Model m) =
    let
        menu =
            Mu.selectForward m.menu
    in
    ( Model { m | menu = menu }
    , Mu.getSelected menu
        |> Maybe.map .text
        |> Maybe.withDefault m.word
    )


{-| return new model and current selected item
-}
selectBackward : Model -> ( Model, String )
selectBackward (Model m) =
    let
        menu =
            Mu.selectBackward m.menu
    in
    ( Model { m | menu = menu }
    , Mu.getSelected menu
        |> Maybe.map .text
        |> Maybe.withDefault m.word
    )


{-| if provider changed, send cmd to download list
, after list downloaded use `updateSource` to update model
-}
updateWord :
    { pathSeperator : String, homedir : String, cwd : String }
    -> String
    -> Model
    -> ( Model, Cmd msg )
updateWord { pathSeperator, homedir, cwd } word (Model m) =
    let
        provider =
            m.provider

        menu =
            word
                |> fuzzyMatch m.source
                |> Mu.init 10

        toPath w =
            w
                |> String.trim
                |> String.split " "
                |> getLast
                |> Maybe.withDefault ""
                |> getPath pathSeperator homedir cwd

        newProvider =
            case provider of
                FileName _ ->
                    FileName <| toPath word

                DirectoryName _ ->
                    DirectoryName <| toPath word

                AllFiles _ ->
                    AllFiles <| toPath word

                WordsInBuffer _ ->
                    provider

                ExHistory ->
                    provider

                BufferNames ->
                    provider
    in
    ( Model { m | provider = newProvider }
    , if newProvider /= provider then
        providerToCmd provider

      else
        Cmd.none
    )


updateSource : List String -> Model -> Model
updateSource source (Model m) =
    Model { m | source = source }


render : Model -> Html msg
render (Model _) =
    div [] []



-- Helpers


getPath : String -> String -> String -> String -> String
getPath sep homedir cwd s1 =
    let
        s =
            replaceHomeDir homedir s1

        base =
            if s == homedir then
                s

            else
                pathBase sep s
    in
    resolvePath sep cwd base
