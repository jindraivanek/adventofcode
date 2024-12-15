module fs.day15

let grid (s: day15.State) =
    let robot = s.Robot, Grid.Player s.Robot
    let walls = s.Walls |> Seq.map (fun p -> p, Grid.Wall)
    let boxes = s.Boxes |> Seq.collect (day15.boxCells s) |> Seq.map (fun p -> p, Grid.Trail)
    {| Grid = Seq.concat [walls; boxes; [robot]] |> Map.ofSeq; Size = Seq.max s.Boxes |> fun (x, y) -> x+1, y+1 |}

let sol =
    let input = Common.readLines 15 "_sample" |> Option.get
    //let input = Common.readLines 15 "" |> Option.get
    let init = day15.sol.Part2.Init input
    let step = day15.sol.Part2.Step
    Common.run init step |> Seq.map grid |> Seq.toArray
