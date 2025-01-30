module day18

open Common
open Common.Grid

let parseInput n (lines: string[]) =
    let wallSeq =
        lines
        |> Seq.map (fun l -> l.Split(',') |> Array.map int |> (fun xs -> xs[0], xs[1]))

    let walls = wallSeq |> Seq.take n |> set

    let starts = [ (0, 0), 0 ]

    let size = if Seq.max walls |> fst > 6 then 71 else 7

    let isInMap (x, y) =
        x >= 0 && x < size && y >= 0 && y < size

    let neighF p =
        let moves =
            dirs
            |> List.map (posPlus p)
            |> List.filter (fun v -> Set.contains v walls |> not && isInMap v)

        moves |> List.map (fun v -> (v, 1))

    let isFinish = (=) (size - 1, size - 1)

    {| Starts = starts
       Walls = walls
       WallsSeq = wallSeq
       NeighF = neighF
       IsFinish = isFinish |}

let dijkstra (initNodes: ('n * int) list) (neighF: 'n -> ('n * int) list) (finishCond: 'n -> bool) resultMap =
    let pq = System.Collections.Generic.PriorityQueue<'n, int>()
    let visited = System.Collections.Generic.HashSet<_>()
    let mutable opt = None

    let dequeue () =
        let (success, n, p) = pq.TryDequeue()

        if success then
            //if visited.Count % 1000 = 0 then printfn "%A" (node, p)
            Some(n, p)
        else
            None

    initNodes |> Seq.iter (fun (n, p) -> pq.Enqueue(n, p))

    let rec step () =
        match dequeue (), opt with
        | Some(node, p), _ when visited.Contains(node) ->
            //printfn $"Already visited: %A{node}, %A{p}"
            step ()
        | None, _ -> None
        //| Some(node, p), Some o when finishCond node && p > o -> result p |> resultMap
        | Some(node, p), None when finishCond node ->
            //printfn $"FINISH %A{node} %A{p}"
            opt <- Some p
            Some(resultMap (p, node))
        | Some((node), p), _ ->
            //printfn $"%A{node}, %A{p}"
            visited.Add(node) |> ignore

            neighF node
            |> Seq.iter (fun (n, p') ->
                let c = p + p'
                pq.Enqueue(n, c))

            step ()
        | None, None -> failwith "No solution"

    step ()


let part1 lines =
    let s = parseInput 1024 lines
    dijkstra s.Starts s.NeighF s.IsFinish fst

let part2 lines =
    Seq.initInfinite (fun i ->
        let s = parseInput (1024 + i) lines
        1024 + i, dijkstra s.Starts s.NeighF s.IsFinish fst, s)
    |> Seq.find (fun (i, x, s) -> x.IsNone)
    |> fun (i, x, s) -> s.WallsSeq |> Seq.take i |> Seq.last


let sol =
    { Day = 18
      Part1 = solution part1 (string)
      Part2 = solution part2 (string) }
