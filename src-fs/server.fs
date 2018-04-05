
(*#r "packages/Suave/lib/net40/Suave.dll"
 *)

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open System.IO

let trace x =
  printfn "%s" x
  x

let edit =
    request (fun r ->
        match r.queryParam "path" with
        | Choice1Of2 path ->
          if File.Exists path then
            Files.sendFile (trace path) false
          else
            NOT_FOUND path
        | Choice2Of2 msg -> BAD_REQUEST msg)

let write =
    request (fun r ->
        match r.queryParam "path" with
        | Choice1Of2 path ->
          File.WriteAllBytes (trace path, r.rawForm)
          OK ""
        | Choice2Of2 msg -> BAD_REQUEST msg)

let app =
  choose
    [ GET >=> choose
        [ path "/" >=> Files.file "index.html"
          Files.browse (Path.GetFullPath ".")
          path "/edit" >=> edit
        ]
      POST >=> choose
        [ path "/write" >=> write ] ]


startWebServer defaultConfig app
