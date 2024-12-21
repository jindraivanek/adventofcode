module day20
open Common
open Common.Grid

let parseInput (lines: string[]) =
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> c, (i, j))) |> Seq.collect id
    let w = m |> Seq.filter (fun (c, _) -> c = '#') |> Seq.map snd |> set
    let f = m |> Seq.filter (fun (c, _) -> c <> '#') |> Seq.map snd |> set
    let starts = m |> Seq.filter (fun (c, _) -> c = 'S') |> Seq.map (snd >> fun p -> p, 0) |> List.ofSeq
    let finish = m |> Seq.filter (fun (c, _) -> c = 'E') |> Seq.map (snd) |> Seq.head

    {| Starts = starts
       Walls = w
       Floors = f
       Finish = finish |}

let getNeighGraph (m :  {| Finish: int * int; Floors: Set<int * int>; Starts: list<(int * int) * int>; Walls: Set<int * int> |}) =
    let neighF p =
        let moves = dirs |> List.map (posPlus p) //|> List.filter (fun v -> Set.contains v m.Floors)
        moves |> List.map (fun v -> (v, 1, Set.contains v m.Walls))

    let isFinish = (=) m.Finish

    {| Starts = m.Starts
       Walls = m.Walls
       Floors = m.Floors
       NeighF = neighF
       IsFinish = isFinish |}

let getGraph (m :  {| Finish: int * int; Floors: Set<int * int>; Starts: list<(int * int) * int>; Walls: Set<int * int> |}) =
    let neighF p =
        let moves = dirs |> List.map (posPlus p) |> List.filter (fun v -> Set.contains v m.Floors)
        moves
    let edges = m.Floors |> Seq.collect (fun p -> neighF p |> Seq.map (fun v -> p, v)) |> Seq.toList
    {| Nodes = Seq.toList m.Floors; Edged = edges |}

let floydWarshall (nodes: seq<'n>) edges =
    let nodes = nodes |> Seq.toArray 
    let n = Seq.length nodes
    let nodeIndex = nodes |> Seq.indexed |> Seq.map (fun (i, v) -> v, i) |> Map.ofSeq
    let index v = nodeIndex[v]
    let dist = Array2D.init n n (fun i j -> if i = j then 0 else 99999)
    for (u, v) in edges |> Seq.map (fun (u, v) -> index u, index v) do
        dist[u,v] <- 1
    for v in 0 .. n-1 do
        dist[v,v] <- 0
    for k in 0 .. n-1 do
        for i in 0 .. n-1 do
            for j in 0 .. n-1 do
                if dist[i,j] > dist[i,k] + dist[k,j] 
                then dist[i,j] <- dist[i,k] + dist[k,j]
    seq {
        for i in 0 .. n-1 do
            for j in 0 .. n-1 do
                yield ((nodes[i], nodes[j]), dist[i,j])
    } |> Map.ofSeq

let pathsWithShortcut k diff start finish nodes (distMap: Map<_,_>) =
    let opt = distMap[start, finish]
    printfn "%A" opt
    nodes |> Seq.collect (fun u ->
        nodes |> Seq.map (fun v -> u, v))
    |> Seq.filter (fun (u, v) -> u <> v && dist u v <= k && distMap[start, u] + dist u v + distMap[v, finish] <= opt - diff)
    |> Seq.map (fun (u, v) -> printfn "%A %A" u, v; u, v)

let dijkstra cheatN runToCost (initNodes: ('n * int) list) (neighF: 'n -> ('n * int * bool) list) heur (finishCond: 'n -> bool) resultMap =
    let pq = System.Collections.Generic.PriorityQueue<('n * (int *int * Set<_>)), int>()
    let visited = System.Collections.Generic.HashSet<_>()
    let cheats = System.Collections.Generic.HashSet<_>()
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
        pq.Enqueue((n, (p, cheatN, set [])), p + heur n))

    let rec step () =
        match dequeue (), runToCost with
        | Some((n, (_,_, ch)) as node, p), (Some c) when finishCond n && p <= c ->
            printfn $"FINISH %A{node} %A{p}"
            opt <- Some p
            ch |> Seq.iter (cheats.Add >> ignore)
            step()
        | Some(node, p), (Some c) when p > c ->
            printfn $"END %A{node} %A{p}"
            Some (resultMap (cheats.Count, opt.Value))
        | Some(node, p), None when visited.Contains(node) ->
            //printfn $"Already visited: %A{node}, %A{p}"
            step ()
        | None, _ -> None
        | Some(node, p), None when finishCond (fst node) ->
            printfn $"FINISH %A{node} %A{p}"
            opt <- Some p
            Some (resultMap (cheats.Count, p))
        | Some((n, (p, ch, chSet)) as node, _), _ ->
            //printfn $"%A{node}, %A{p}"
            visited.Add(node) |> ignore

            neighF n
            |> Seq.iter (fun (n, p', isCheat) ->
                let c = p + p'
                let c_h = c + heur n
                if (isCheat && ch = cheatN && cheatN > 0) then
                    //printfn "CHEAT %A" (n, c)
                    pq.Enqueue((n, (c, ch - 1, Set.add n chSet)), c_h)
                elif (isCheat || ch < cheatN) && ch > 0 then
                    pq.Enqueue((n, (c, ch - 1, chSet)), c_h)
                elif not isCheat then 
                    pq.Enqueue((n, (c, ch, chSet)), c_h)
            )

            step ()

    step ()


let part1 lines = 
   let m = parseInput lines
   let s = getNeighGraph m
   let heur p = dist m.Finish p
   //let opt = dijkstra 0 None s.Starts s.NeighF heur s.IsFinish snd |> Option.get
//    m.Walls |> Seq.map (fun w -> 
//         let s = getGraph {| m with Walls = Set.remove w m.Walls; Floors = Set.add w m.Floors |}
//         dijkstra s.Starts s.NeighF s.IsFinish fst |> Option.get
//    ) |> Seq.filter (fun x -> opt - x >= 100) |> Seq.length
   //dijkstra 2 (Some (opt-100)) s.Starts s.NeighF heur s.IsFinish fst |> Option.get
   -1

let part2 lines = 
    let m = parseInput lines
    let g = getGraph m
    let d = floydWarshall g.Nodes g.Edged
    pathsWithShortcut 20 100 (Seq.head m.Starts |> fst) m.Finish g.Nodes d

let sol = {
    Day = 20
    Part1 = solution part1 (string) //1343
    Part2 = solution part2 (Seq.length >> string)
}
