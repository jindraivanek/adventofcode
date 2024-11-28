let scripts =
    System.IO.DirectoryInfo(__SOURCE_DIRECTORY__).GetDirectories()
    |> Seq.collect (fun d ->
        System.IO.DirectoryInfo(d.FullName).GetFiles("solve.fsx")
        |> Seq.map (fun x -> d, x.FullName))

scripts
|> Seq.iter (fun (d, x) ->
    let targetPath = $"../fsharp-proj/%s{d.Name}.fs"
    System.IO.File.ReadAllText(x)
    |> fun s -> s.Replace("""#time""", $"""module day%s{d.Name}""")
    |> fun s -> System.IO.File.WriteAllText(targetPath, s)
    )
    
