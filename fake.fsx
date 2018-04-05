#r "packages/FAKE/tools/FakeLib.dll"
open Fake

let outputFile = "dist/server.exe"

Target "Build" (fun _ ->
    !!("src-fs" @@ "**/*.fs")
    |> Seq.map StringHelper.toRelativePath
    |> Seq.toList
    |> FscHelper.compile [
        FscHelper.References [ "packages/Suave/lib/net40/Suave.dll" ]
        FscHelper.Out outputFile
        FscHelper.Standalone
    ]
    |> function 0 -> () | c -> failwithf "F# compiler return code: %i" c
)

Target "Clean" (fun _ ->
    !!("bin" @@ "**/*.exe")
    |> FileHelper.DeleteFiles
)

Target "Run" (fun _ ->
  Shell.Exec outputFile
    |> ignore
  ()
)

"Build" ==> "Run"

// start build
RunTargetOrDefault "Build"
