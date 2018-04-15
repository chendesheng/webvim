
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
open System.Text

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

let elmFormat (input : String) =
    try
        let p = new Process()
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardInput <- true;
        p.StartInfo.RedirectStandardOutput <- true;
        p.StartInfo.FileName <- "elm-format"
        p.StartInfo.Arguments <- "--stdin"
        p.Start()
        p.StandardInput.Write(input)
        p.StandardInput.Close()
        let output = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode = 0 && input <> output
        then
            p.Close()
            Some output
        else
            p.Close()
            None
    with :? Exception as e ->
        printfn "[elm-format] error: %s" e.Message
        None

let elmLint file =
    try
        trace "elm lint"
        let p = new Process()
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardInput <- true;
        p.StartInfo.RedirectStandardOutput <- true;
        p.StartInfo.FileName <- "elm-make"
        p.StartInfo.Arguments <-
            (file + " --yes --warn --report=json --output=/dev/null")

        p.Start()
        let result = p.StandardOutput.ReadToEnd() |> trace
        p.WaitForExit()
        if p.ExitCode = 0
        then
            p.Close()
            Some "[]"
        else
            p.Close()
            Some result
    with :? Exception as e ->
        printfn "[elm-make] error: %s" e.Message
        None

let lintTempFile = Path.GetRandomFileName()

let elmLintOnTheFly content =
    let name = Path.Combine(Path.GetTempPath(), lintTempFile)
    File.WriteAllBytes (name, content)
    elmLint name

let lint =
    request (fun r ->
        match elmLint "src/Main.elm" with
        | Some json -> OK json
        | None -> OK "")

let lintOnTheFly =
    request (fun r ->
        match elmLintOnTheFly r.rawForm with
        | Some json -> OK json
        | None -> OK "")

    
let write =
    request (fun r ->
        match r.queryParam "path" with
        | Choice1Of2 p ->
          if p.EndsWith(".elm", StringComparison.InvariantCultureIgnoreCase)
          then
              trace "elm format"
              let text = Encoding.UTF8.GetString r.rawForm
              match elmFormat text with
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
          path "/lint" >=> lint
        ]

      POST >=> choose
        [ path "/write" >=> write
          path "/lint" >=> lintOnTheFly
        ]

    ]

startWebServer defaultConfig app
