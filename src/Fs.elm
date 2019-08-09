module Fs exposing
    ( FileSystem
    , absolutePath
    , cd
    , fileSystem
    , homeDir
    , invalidFileSystem
    , listAllFiles
    , listDirs
    , listFiles
    , mkDir
    , pathSeperator
    , read
    , search
    , setWorkingDir
    , shortPath
    , workingDir
    , write
    )

import Dict exposing (Dict)
import Helper.Helper exposing (..)
import Http
import Task exposing (Task)


type FileSystem
    = Fs
        { url : String
        , sep : String
        , home : String
        , cwd : String
        }


invalidFileSystem : FileSystem
invalidFileSystem =
    Fs { url = "", sep = "/", home = "", cwd = "" }


fileSystem : String -> String -> String -> String -> FileSystem
fileSystem url sep home cwd =
    Fs
        { url = url
        , sep = sep
        , home = home
        , cwd =
            if cwd == "" then
                home

            else
                cwd
        }


homeDir : FileSystem -> String
homeDir (Fs { home }) =
    home


workingDir : FileSystem -> String
workingDir (Fs { cwd }) =
    cwd


setWorkingDir : String -> FileSystem -> FileSystem
setWorkingDir cwd (Fs fs) =
    Fs { fs | cwd = cwd }


pathSeperator : FileSystem -> String
pathSeperator (Fs { sep }) =
    sep


search : FileSystem -> String -> Cmd (Result String String)
search (Fs { url, cwd }) s =
    Http.get
        { url = url ++ "/search?s=" ++ s ++ "&cwd=" ++ cwd
        , expect =
            Http.expectString <|
                Result.mapError httpErrorMessage
        }


cd : FileSystem -> String -> Cmd (Result String String)
cd (Fs { url }) cwd =
    Http.get
        { url = url ++ "/cd?cwd=" ++ cwd
        , expect =
            Http.expectString <|
                Result.mapError httpErrorMessage
        }


mkDir : FileSystem -> String -> Cmd (Result String ())
mkDir (Fs { url }) path =
    Http.get
        { url = url ++ "/mkdir?path=" ++ path
        , expect =
            Http.expectString
                (Result.mapError httpErrorMessage
                    >> Result.map (always ())
                )
        }


listFiles : FileSystem -> String -> Cmd (Result String (List String))
listFiles (Fs { url, sep }) cwd =
    let
        trimSep s =
            if String.toLower s == ".ds_store" then
                Nothing

            else
                Just <| s
    in
    Http.get
        { url = url ++ "/ld?cwd=" ++ cwd
        , expect =
            Http.expectString <|
                parseFileList sep
                    >> Result.map (List.filterMap trimSep)
        }


listDirs : FileSystem -> String -> Cmd (Result String (List String))
listDirs (Fs { url, sep }) cwd =
    let
        trimSep s =
            if s == "./" || s == "../" then
                Nothing

            else if String.endsWith sep s then
                Just <| String.dropRight (String.length sep) s

            else
                Nothing
    in
    Http.get
        { url = url ++ "/ld?cwd=" ++ cwd
        , expect =
            Http.expectString <|
                parseFileList sep
                    >> Result.map (List.filterMap trimSep)
        }


listAllFiles : FileSystem -> Cmd (Result String (List String))
listAllFiles (Fs { url, sep, cwd }) =
    Http.get
        { url = url ++ "/ls?cwd=" ++ cwd
        , expect = Http.expectString <| parseFileList sep
        }


lastModified : Dict String String -> String
lastModified headers =
    Dict.get "last-modified" headers
        |> Maybe.withDefault ""


write : FileSystem -> String -> String -> Cmd (Result String ( String, String ))
write (Fs { url }) path s =
    Http.post
        { url = url ++ "/write?path=" ++ path
        , body = Http.stringBody "text/plain" s
        , expect =
            Http.expectStringResponse identity <|
                (httpStringResponse
                    >> Result.map (\( headers, body ) -> ( lastModified headers, body ))
                    >> Result.mapError httpErrorMessage
                )
        }


read : FileSystem -> String -> Task String (Maybe ( String, String ))
read (Fs { url }) path =
    { method = "GET"
    , headers = []
    , url = url ++ "/read?path=" ++ path
    , body = Http.emptyBody
    , resolver =
        Http.stringResolver <|
            httpStringResponse
                >> (\result ->
                        case result of
                            Ok ( headers, body ) ->
                                Ok <| Just ( lastModified headers, body )

                            Err (Http.BadStatus 404) ->
                                -- 404 is ok here, the buffer was created already
                                Ok Nothing

                            Err err ->
                                Err (httpErrorMessage err)
                   )
    , timeout = Nothing
    }
        |> Http.task


absolutePath : FileSystem -> String -> String
absolutePath (Fs { sep, home, cwd }) s1 =
    let
        s =
            replaceHomeDir home s1
    in
    resolvePath sep cwd s


shortPath : FileSystem -> String -> String
shortPath (Fs { sep, cwd }) path =
    relativePath sep cwd path
