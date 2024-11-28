let scripts =
    System.IO.DirectoryInfo(__SOURCE_DIRECTORY__).GetDirectories()
    |> Seq.collect (fun d ->
        System.IO.DirectoryInfo(d.FullName).GetFiles("solve.fsx")
        |> Seq.map (fun x -> d, x.FullName))

scripts
|> Seq.iter (fun (d, x) ->
    System.IO.File.ReadAllText(x) 
    |> fun s -> s.Replace("""System.IO.File.ReadAllLines("input")""", $"""System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/{d.Name}.txt")""")
    |> fun s -> System.IO.File.WriteAllText(x, s)
    )
    
