
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
open System.Diagnostics

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

let elmFormat (content : byte[]) =
    try
        let p = new Process()
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardInput <- true;
        p.StartInfo.RedirectStandardOutput <- true;
        p.StartInfo.FileName <- "elm-format"
        p.StartInfo.Arguments <- "--stdin"
        p.Start()
        p.StandardInput.BaseStream.Write(content, 0, content.Length)
        p.StandardInput.Close()
        let result = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode = 0
        then
            p.Close()
            Some result
        else
            p.Close()
            None
    with :? Exception as e ->
        printfn "[elm-format] error: %s" e.Message
        None
    
let write =
    request (fun r ->
        match r.queryParam "path" with
        | Choice1Of2 p ->
          if p.EndsWith(".elm", StringComparison.InvariantCultureIgnoreCase)
          then
              trace "elm format"
              match elmFormat r.rawForm with
              | Some formatted ->
                  File.WriteAllText (p, formatted)
                  OK formatted
              | None ->
                  File.WriteAllBytes (p, r.rawForm)
                  OK ""
          else
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
