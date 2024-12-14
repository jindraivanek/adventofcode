module fs.day14

let grid (s: day14.State) =
    let robots = s.Robots |> Seq.map (fun (p, _) -> p, Grid.Wall)
    {| Grid = Seq.concat [robots] |> Map.ofSeq; Size = s.Size; PeekHighlight = true |}

let sol =
    let input = Common.readLines 14 "_sample" |> Option.get
    let input = Common.readLines 14 "" |> Option.get
    let init = day14.sol.Part2.Init input
    let step = day14.sol.Part2.Step
    Common.run init step |> Seq.map grid |> Seq.toArray
