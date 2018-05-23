
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
open System.Collections.Generic

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

let elmLint (dir: String) (file : String) =
    try
        trace "elm lint"
        let p = new Process()
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardOutput <- true
        p.StartInfo.RedirectStandardError <- true
        p.StartInfo.FileName <- "elm-make"
        p.StartInfo.WorkingDirectory <- dir
        p.StartInfo.Arguments <-
            (file + " --yes --warn --report=json --output=/dev/null")

        p.Start()
        let result = p.StandardOutput.ReadToEnd() |> trace
        trace <| sprintf "%d %d" p.ExitCode result.Length
        if p.ExitCode = 0 then
            p.WaitForExit()
            p.Close()
            Some "[]"
        else if result.Length = 0 then
            let errResult = p.StandardError.ReadToEnd()
            p.WaitForExit()
            p.Close()
            Some errResult
        else
            p.WaitForExit()
            p.Close()
            Some result
    with :? Exception as e ->
        printfn "[elm-make] error: %s" e.Message
        None

let lintTempFile = Path.GetRandomFileName()

let elmLintDir (r : HttpRequest) =
  match r.queryParam "path" with
  | Choice1Of2 p ->
    if p.StartsWith("tests/") then "tests" else "" 
  | Choice2Of2 _ ->
    ""
  

let elmLintOnTheFly r =
    let name = Path.Combine(Path.GetTempPath(), lintTempFile)
    File.WriteAllBytes (name, r.rawForm)
    elmLint (elmLintDir r) name

let lint =
    request (fun r ->
        match r.queryParam "path" with
        | Choice1Of2 p ->
          let file = if p.StartsWith("tests/") then
                        p.Substring(6)
                     else p //"src/Main.elm"
          let dir = elmLintDir r 
          match elmLint dir file with
          | Some json -> OK json
          | None -> OK ""
        | Choice2Of2 msg -> BAD_REQUEST msg)

let lintOnTheFly =
    request (fun r ->
        match elmLintOnTheFly r with
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


let ag args =
    try
        trace ("ag " + args)
        let p = new Process()
        p.StartInfo.UseShellExecute <- false
        p.StartInfo.RedirectStandardOutput <- true;
        p.StartInfo.RedirectStandardError <- true;
        p.StartInfo.FileName <- "ag"
        p.StartInfo.Arguments <- "-l --nocolor"

        p.Start()

        let result = p.StandardOutput.ReadToEnd()

        p.WaitForExit()
        p.Close()
        Some result
    with :? Exception as e ->
        printfn "[ag] error: %s" e.Message
        None

let listFiles =
    request (fun r ->
        match ag "-l --nocolor" with
        | Some s -> OK s
        | None -> OK "")


let logger = Targets.create Verbose [||]

let noCache = 
  setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >=> setHeader "Pragma" "no-cache"
  >=> setHeader "Expires" "0"

let app =
  CORS.cors { CORS.defaultCORSConfig with allowedUris = CORS.All }
  >=> logStructured logger logFormatStructured
  >=> choose
    [ GET >=> choose
        [ path "/" >=> Files.file "index.html" >=> noCache
          Files.browse (Path.GetFullPath ".")
          path "/edit" >=> edit >=> noCache
          path "/kill" >=> (fun x ->
              trace "Bye!"
              Environment.Exit(0)
              OK "" x) >=> noCache
          path "/lint" >=> lint >=> noCache
          path "/ls" >=> listFiles >=> noCache
        ]

      POST >=> choose
        [ path "/write" >=> write
          path "/lint" >=> lintOnTheFly
        ]

    ]

startWebServer defaultConfig app
