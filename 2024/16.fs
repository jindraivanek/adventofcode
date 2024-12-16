module day16
open Common
open Common.Grid

let (%) x y = ((x % y) + y) % y

let parseInput (lines: string[]) =
    let m =
        lines
        |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> (i, j), c))
        |> Seq.collect id
        |> Map.ofSeq

    let starts =
        m
        |> Map.filter (fun _ v -> v = 'S')
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.map (fun p -> (p, 1), 0)
        |> List.ofSeq

    let finish = m |> Map.filter (fun _ v -> v = 'E') |> Map.toSeq |> Seq.map fst |> set
    let floors = m |> Map.filter (fun _ v -> v <> '#') |> Map.toSeq |> Seq.map fst |> set
    let dir i = dirs[i % dirs.Length]

    let neighF (p, di) =
        let d = dir di
        [(posPlus p d, di), 1; (p, di + 1), 1000; (p, di - 1), 1000]
        |> List.filter (fun ((v, _), _) -> Set.contains v floors)

    let isFinish (p, _) = Set.contains p finish

    {| Starts = starts
       NeighF = neighF
       IsFinish = isFinish |}

let dijkstra (initNodes: ('n * int) list) (neighF: 'n -> ('n * int) list) (finishCond: 'n -> bool) resultMap =
    let pq = System.Collections.Generic.PriorityQueue<'n, int>()
    let visited = System.Collections.Generic.HashSet<_>()
    let score = System.Collections.Generic.Dictionary<_, _ Set>()

    let dequeue () =
        let (success, n, p) = pq.TryDequeue()

        if success then
            //if visited.Count % 1000 = 0 then printfn "%A" (node, p)
            Some(n, p)
        else
            None

    initNodes
    |> Seq.iter (fun (n, p) ->
        pq.Enqueue(n, p)
        score.Add(n, set [ [ n ] ]))

    let rec step () =
        match dequeue () with
        | Some((node), p) when visited.Contains(node) -> step ()
        | None ->
            score
            |> Seq.filter (fun kvp -> finishCond kvp.Key)
            |> Seq.sumBy (fun kvp -> kvp.Value |> Set.map resultMap |> Set.count)
        | Some((node), p) when finishCond node -> p
        | Some((node), p) ->
            visited.Add(node) |> ignore

            neighF node
            |> Seq.iter (fun (n, p') ->
                let newScore = score[node] |> Set.map (fun p -> n :: p)

                if score.ContainsKey n then
                    score[n] <- Set.union score[n] newScore
                else
                    score.Add(n, newScore)

                pq.Enqueue(n, p + p')

            //if finishCond n then
            //printfn $"%A{(n, score[n])}"
            )

            step ()

    step ()


let part1 lines =
    let s = parseInput lines
    let r = dijkstra s.Starts s.NeighF s.IsFinish List.last
    r

let part2 lines =
    let s = parseInput lines
    let r = dijkstra s.Starts s.NeighF s.IsFinish id
    r


let sol = {
    Day = 16
    Part1 = solution part1 string
    Part2 = solution part2 (string)
}
