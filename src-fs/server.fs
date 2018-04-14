
(*#r "packages/Suave/lib/net40/Suave.dll"
 *)

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open System.IO
open System
open Suave.Logging
open Suave.Writers

let trace x =
  printfn "%s" x
  x

let edit =
    request (fun r ->
        match r.queryParam "path" with
        | Choice1Of2 p ->
          if File.Exists p then
            Files.sendFile p false
          else
            NOT_FOUND p
        | Choice2Of2 msg -> BAD_REQUEST msg)

let write =
    request (fun r ->
        match r.queryParam "path" with
        | Choice1Of2 p ->
          File.WriteAllBytes (p, r.rawForm)
          OK ""
        | Choice2Of2 msg -> BAD_REQUEST msg)

let logger = Targets.create Verbose [||]

let app =
  CORS.cors { CORS.defaultCORSConfig with allowedUris = CORS.All }
  >=> logStructured logger logFormatStructured
  >=> choose
    [ GET >=> choose
        [ path "/" >=> Files.file "index.html"
          Files.browse (Path.GetFullPath ".")
          path "/edit" >=> edit
          path "/kill" >=> (fun x ->
              trace "Bye!"
              Environment.Exit(0)
              OK "" x)
        ]

      POST >=> choose
        [ path "/write" >=> write ]

    ]

startWebServer defaultConfig app
