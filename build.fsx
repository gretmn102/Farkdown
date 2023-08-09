// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let f projName =
    let pattern = sprintf @"**/%s.fsproj" projName
    let xs = !! pattern
    xs
    |> Seq.tryExactlyOne
    |> Option.defaultWith (fun () ->
        xs
        |> List.ofSeq
        |> failwithf "'%s' expected exactly one but:\n%A" pattern
    )

let mainProjName = "Farkdown"
let mainProjPath = f mainProjName
let mainProjDir = Path.getDirectory mainProjPath

let testProjName = "Tests"
let testProjPath = f testProjName
let testsProjDir = Path.getDirectory testProjPath

let deployDir = Path.getFullName "./deploy"

let release = ReleaseNotes.load "RELEASE_NOTES.md"
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

module XmlText =
    let escape rawText =
        let doc = new System.Xml.XmlDocument()
        let node = doc.CreateElement("root")
        node.InnerText <- rawText
        node.InnerXml
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
Target.create "Clean" (fun _ ->
    Shell.cleanDir deployDir
    dotnet "clean" "."
)

Target.create "Meta" (fun _ ->
    [
        "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
        "<ItemGroup>"
        "    <PackageReference Include=\"Microsoft.SourceLink.GitHub\" Version=\"1.0.0\" PrivateAssets=\"All\"/>"
        "</ItemGroup>"
        "<PropertyGroup>"
        "    <EmbedUntrackedSources>true</EmbedUntrackedSources>"
        "    <PackageProjectUrl>https://github.com/gretmn102/Farkdown</PackageProjectUrl>"
        "    <PackageLicenseExpression>MIT</PackageLicenseExpression>"
        "    <RepositoryUrl>https://github.com/gretmn102/Farkdown.git</RepositoryUrl>"
        sprintf "    <PackageReleaseNotes>%s</PackageReleaseNotes>"
            (String.concat "\n" release.Notes |> XmlText.escape)
        "    <PackageTags>fsharp;markdown</PackageTags>"
        "    <Authors>gretmn102</Authors>"
        sprintf "    <Version>%s</Version>" (string release.SemVer)
        "</PropertyGroup>"
        "</Project>"
    ]
    |> File.write false "Directory.Build.props"
)

let commonBuildArgs = "-c Release"

Target.create "Build" (fun _ ->
    mainProjDir
    |> dotnet (sprintf "build %s" commonBuildArgs)
)

Target.create "Deploy" (fun _ ->
    mainProjDir
    |> dotnet (sprintf "build %s -o \"%s\"" commonBuildArgs deployDir)
)

Target.create "Pack" (fun _ ->
    mainProjDir
    |> dotnet (sprintf "pack %s -o \"%s\"" commonBuildArgs deployDir)
)

Target.create "PushToGitlab" (fun _ ->
    let packPathPattern = sprintf "%s/*.nupkg" deployDir
    let packPath =
        !! packPathPattern |> Seq.tryExactlyOne
        |> Option.defaultWith (fun () -> failwithf "'%s' not found" packPathPattern)

    deployDir
    |> dotnet (sprintf "nuget push -s %s %s" "gitlab" packPath)
)

Target.create "BuildTests" (fun _ ->
    testsProjDir
    |> dotnet (sprintf "build %s" commonBuildArgs)
)

Target.create "RunTests" (fun _ ->
    testsProjDir
    |> dotnet (sprintf "run %s" commonBuildArgs)
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"Build"

"Clean"
  ==> "Deploy"

"Clean"
  ==> "Meta"
  ==> "Pack"
  ==> "PushToGitlab"

"BuildTests"

"RunTests"

Target.runOrDefault "Deploy"
