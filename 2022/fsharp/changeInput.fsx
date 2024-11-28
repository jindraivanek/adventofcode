let scripts =
    System.IO.DirectoryInfo(__SOURCE_DIRECTORY__).GetFiles("day*.fsx")
        |> Seq.map (fun x -> x.FullName)

scripts
//|> Seq.take 1
|> Seq.iter (fun x ->
    printfn "%s" x
    let r = System.Text.RegularExpressions.Regex("""System.IO.File.ReadAllLines\("day([0-9]*)\.input"\)""")
    System.IO.File.ReadAllText(x) 
    |> fun s -> 
        let m = r.Match(s)
        if m.Success then
            printfn $"%A{r.Match(s).Groups.[1].Value}"
            let d = r.Match(s).Groups.[1].Value |> int
            printfn "%A" (r.Match(s).Value, $"""System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2022/%02i{d}.txt")""")
            s.Replace(r.Match(s).Value, $"""System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2022/%02i{d}.txt")""")
            |> fun s -> System.IO.File.WriteAllText(x, s)
    )
    
