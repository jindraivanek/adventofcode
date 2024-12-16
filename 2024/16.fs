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
        let moves = [((posPlus p d), di), 1] |> List.filter (fun ((v, _), _) -> Set.contains v floors)
        let rotates = [(p, (di + 1) % dirs.Length), 1000; (p, (di - 1) % dirs.Length), 1000] |> List.filter (fun ((p, i), _) -> Set.contains (posPlus p (dir i)) floors)
        moves @ rotates

    let isFinish (p, _) = Set.contains p finish

    {| Starts = starts
       NeighF = neighF
       IsFinish = isFinish |}

let dijkstra (initNodes: ('n * int) list) (neighF: 'n -> ('n * int) list) (finishCond: 'n -> bool) resultMap =
    let pq = System.Collections.Generic.PriorityQueue<'n, int>()
    let visited = System.Collections.Generic.HashSet<_>()
    let paths = System.Collections.Generic.Dictionary<_, _ * _ Set>()
    let finishPaths = System.Collections.Generic.HashSet<_>()
    let mutable opt = None

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
        let (v, _) = n
        paths.Add(n, (p, set [ v ])))

    let result p = p, finishPaths |> Seq.collect id |> set |> Set.count 
    
    let rec step () =
        match dequeue (), opt with
        | Some(node, p), _ when visited.Contains(node) ->
            //printfn $"Already visited: %A{node}, %A{p}"
            step ()
        | None, Some o -> result o |> resultMap
        | Some(node, p), Some o when finishCond node && p > o -> result p |> resultMap
        | Some(node, p), None when finishCond node ->
            //printfn $"FINISH %A{node} %A{p}"
            opt <- Some p
            finishPaths.Add(snd paths[node]) |> ignore
            step()
        | Some((node), p), _ ->
            //printfn $"%A{node}, %A{p}"
            visited.Add(node) |> ignore

            neighF node
            |> Seq.iter (fun ((v,_) as n, p') ->
                let newPath = Set.add v (snd paths[node])
                let c = p + p'
                
                if paths.ContainsKey n then
                    if c = fst paths[n] then
                        paths[n] <- c, snd paths[n] + newPath
                else
                    paths.Add(n, (c, newPath))

                pq.Enqueue(n, c)
            )

            step ()
        | None, None -> failwith "No solution"

    step ()


let part1 lines =
    let s = parseInput lines
    let r = dijkstra s.Starts s.NeighF s.IsFinish fst
    r

let part2 lines =
    let s = parseInput lines
    let r = dijkstra s.Starts s.NeighF s.IsFinish snd
    r


let sol = {
    Day = 16
    Part1 = solution part1 string
    Part2 = solution part2 (string) //549 too high, 429 too low
}
