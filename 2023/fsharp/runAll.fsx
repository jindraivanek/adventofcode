#r "nuget: CliWrap"

let scripts =
    System.IO.DirectoryInfo(__SOURCE_DIRECTORY__).GetDirectories()
    |> Seq.filter (fun d -> d.Name <> "template")
    |> Seq.sortBy (_.Name)
    |> Seq.collect (fun d ->
        System.IO.DirectoryInfo(d.FullName).GetFiles("solve.fsx")
        |> Seq.map (fun x -> d, x.FullName))

scripts
|> Seq.iter (fun (d, x) ->
    let sb = System.Text.StringBuilder()
    printfn $"DAY {d.Name}"

    CliWrap.Cli
        .Wrap("dotnet")
        .WithArguments($"fsi \"{x}\"")
        .WithWorkingDirectory(d.FullName)
        .WithStandardOutputPipe(CliWrap.PipeTarget.ToStringBuilder(sb))
        .ExecuteAsync()
        .Task.Result
    |> ignore

    printfn "%s" <| sb.ToString())
