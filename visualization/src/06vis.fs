module fs.day06

let grid (s: day06.State) =
    let floor = s.Floor |> Set.map (fun (x, y) -> (x, y), Grid.Floor) |> Set.toSeq
    let walls = s.Wall |> Set.map (fun (x, y) -> (x, y), Grid.Wall) |> Set.toSeq
    let player = (s.Guard, Grid.Player(day06.dirs[s.GuardDir])) |> Seq.singleton
    let trail = s.VisitedRoute |> Set.map (fun ((x, y), d) -> (x, y), Grid.Trail)
    let peek = if s.IsLoop then Set.empty else s.Sim |> Set.map (fun ((x, y), d) -> (x, y), Grid.Peek)
    let peekLoop = if not s.IsLoop then Set.empty else s.Sim |> Set.map (fun ((x, y), d) -> (x, y), Grid.PeekHighlight)
    {| Floor = floor; Grid = Seq.concat [floor; walls; player; peek; trail; peekLoop ] |> Map.ofSeq; Size = s.MapSize; PeekHighlight = s.IsLoop |}

let sol =
    let input = Common.readLines 6 "_sample" |> Option.get
    let input = Common.readLines 6 "" |> Option.get
    let init = day06.part2Sol.Init input
    let step = day06.part2Sol.Step
    Common.run init step |> Seq.map grid |> Seq.toArray
