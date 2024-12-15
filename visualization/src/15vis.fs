module fs.day15

let grid (s: day15.State) =
    let robot = s.Robot, Grid.Player s.Robot
    let walls = s.Walls |> Seq.map (fun p -> p, Grid.Wall)
    let boxes = day15.boxesMap s |> Map.toSeq |> Seq.collect snd |> Seq.map (fun p -> p, Grid.Trail)
    let boxesHead = s.Boxes |> Seq.map (Set.minElement) |> Seq.map (fun p -> p, Grid.PeekHighlight)
    {| Grid = Seq.concat [walls; boxes; boxesHead; [robot]] |> Map.ofSeq; Label = sprintf "%A" (Seq.tryHead s.Instructions); Size = Seq.max boxes |> fun ((x, y), _) -> x+1, y+1 |}

let sol =
    let input = Common.readLines 15 "_sample" |> Option.get
    let input = Common.readLines 15 "" |> Option.get
    let init = day15.sol.Part2.Init input
    let step = day15.sol.Part2.Step
    Common.run init step |> Seq.map grid |> Seq.toArray
