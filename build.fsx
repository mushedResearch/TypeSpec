#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open System

open Fake.Core
open Fake.DotNet
open Fake.IO

let project = "TypeSpec"
let summary = "Versionable Type Contracts"
let gitOwner = "musheddev"
let gitHome = "https://github.com/" + gitOwner
let gitName = "TypeSpec"
let release = ReleaseNotes.load "RELEASE_NOTES.md"

let srcPath = Path.getFullName "./src/TypeSpec"

module Util =
    let join pathParts =
        Path.Combine(Array.ofSeq pathParts)

    let runDotNet cmd workingDir =
        let result =
            DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
        if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

    // let normalizeVersion (version: string) =
    //     let i = version.IndexOf("-")
    //     if i > 0 then version.Substring(0, i) else version

    // let assemblyInfo projectDir extra =
    //     let asmInfoPath = projectDir </> "AssemblyInfo.fs"
    //     (AssemblyInfo.Version release.AssemblyVersion) :: extra
    //     |> AssemblyInfoFile.createFSharp asmInfoPath

    let getAssemblyInfoAttributes projectName =
        [ AssemblyInfo.Title projectName
          AssemblyInfo.Product project
          AssemblyInfo.Description summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion ]
          


    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

Target.create "AssemblyInfo" (fun _ ->
    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
            match projFileName with
            | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
            | Csproj -> AssemblyInfoFile.createCSharp ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
            | Shproj -> ()
        )
)

Target.create "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    -- "src/netcore/**/*.??proj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin/Release", "bin" </> (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
)

Target.create "Clean" (fun _ ->
    Shell.cleanDirs ["bin"; "temp"]
)

Target.create "CleanDocs" (fun _ ->
    Shell.cleanDirs ["docs/output"]
)

Target.create "Restore" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |> Seq.iter (DotNet.restore id))

Target.create "Build" (fun _ ->
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |> Seq.iter (DotNet.build (fun options ->
        { options with 
            Configuration = DotNet.BuildConfiguration.Release
            Common = { options.Common with 
                        CustomParams = Some "--no-restore" } })))

Target.create "RunTests" (fun _ ->
    let restore =
        DotNet.restore id
    let build =
        DotNet.build (fun options ->
        { options with 
            Configuration = DotNet.BuildConfiguration.Release
            Common = { options.Common with 
                        CustomParams = Some "--no-restore" } })
    let runTests (project : string) =
        restore project
        build project
        DotNet.test (fun options ->
            { options with
                Configuration = DotNet.BuildConfiguration.Release
                Common = { options.Common with
                            CustomParams = Some "--no-build -v=normal" } }) project
    runTests "tests/FSharp.Data.GraphQL.Tests/FSharp.Data.GraphQL.Tests.fsproj")



let pack id =
    Shell.cleanDir <| sprintf "nuget/%s.%s" project id
    Paket.pack(fun p ->
        { p with
            Version = release.NugetVersion
            OutputPath = sprintf "nuget/%s.%s" project id
            TemplateFile = sprintf "src/%s.%s/%s.%s.fsproj.paket.template" project id project id
            MinimumFromLockFile = true
            IncludeReferencedProjects = false })

let publishPackage id =
    pack id
    Paket.push(fun p ->
        { p with 
            WorkingDir = sprintf "nuget/%s.%s" project id
            PublishUrl = "https://www.nuget.org/api/v2/package" })



Target.create "Publish" (fun _ ->
    publishPackage "TypeSpec"
)

open Fake.Core.TargetOperators

"Clean"
    ==> "AssemblyInfo"
    ==> "Build"


"Build"
    ==> "RunTests"

"Build"
    ==>
    "Release"    


Target.runOrDefault "Build"
