module day21

open Common
open Common.Grid
open Common.Graph

let keypadString =
    """
789
456
123
#0A
"""

let dirKeypadString =
    """
#^A
<v>
"""

let dirPadMoves =
    function
    | 'A', 'A' -> "A"
    | 'A', '^' -> "<A"
    | 'A', '>' -> "vA"
    | 'A', 'v' -> "v<A"
    | 'A', '<' -> "v<<A"
    | '^', 'A' -> ">A"
    | '^', '^' -> "A"
    | '^', '>' -> "v>A"
    | '^', 'v' -> "vA"
    | '^', '<' -> "v<A"
    | '>', 'A' -> "^A"
    | '>', '^' -> "^<A"
    | '>', '>' -> "A"
    | '>', 'v' -> "<A"
    | '>', '<' -> "<<A"
    | 'v', 'A' -> "^>A"
    | 'v', '^' -> "^A"
    | 'v', '>' -> ">A"
    | 'v', 'v' -> "A"
    | 'v', '<' -> "<A"
    | '<', 'A' -> ">>^A"
    | '<', '^' -> ">^A"
    | '<', '>' -> ">>A"
    | '<', 'v' -> ">A"
    | '<', '<' -> "A"

let dirToChar = Seq.zip dirs "^>v<" |> Map.ofSeq
let dirOrder = [ '^'; '>'; 'v'; '<' ]
let dirs = dirs |> Seq.sortBy (fun d -> Seq.findIndex ((=) dirToChar[d]) dirOrder) |> Seq.toList

let oneLayer s =
    ("A" + s) |> Seq.pairwise |> Seq.map (dirPadMoves) |> String.concat ""

let pairLenMem = memoizeRec <| fun recF (n, a, b, start) ->
    let s = dirPadMoves (a, b)
    //printfn "%A -> %A" (a, b) s
    if n = 1 || s.Length = 1 then s.Length, Seq.head s, Seq.last s
    else 
        let xs =
            string start + s |> Seq.pairwise 
            |> Seq.mapWithState (fun start (a,b) -> recF ((n-1), a, b, start) |> fun (x, s, e) -> e, (x, s, e)) start
            |> Seq.toList
        let first = xs |> List.head |> fun (_,c,_) -> c
        let last = xs |> List.last |> fun (_,_,c) -> c
        let s1 = xs |> Seq.sumBy (fun (a,_,_) -> a)
        s1, first, Seq.last s
        

let layerN n s = 
    //("" + s) |> Seq.pairwise |> Seq.mapWithState (fun start (a,b) -> pairLenMem ((n-1), a, b, start) |> fun (x, s, e) -> e, x) 'A' |> Seq.sum
    ("A" + s) |> Seq.pairwise |> Seq.mapi (fun i (a, b) -> pairLenMem (n, a, b, a) |> fun (x, s, e) -> x) |> Seq.sum

let getGraph (s: string) =
    let lines = s.Split(System.Environment.NewLine)

    let m =
        lines
        |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> (i, j), c))
        |> Seq.collect id
        |> Seq.filter (fun (_, c) -> c <> '#')
        |> Map.ofSeq

    let neighF p =
        let moves =
            dirs |> List.map (posPlus p) |> List.filter (fun v -> Map.containsKey v m)

        moves

    let edges =
        m
        |> Map.keys
        |> Seq.collect (fun p -> neighF p |> Seq.map (fun v -> p, v))
        |> Seq.toList

    {| Map = m |> Map.toSeq |> Seq.map (fun (k, v) -> (v, k)) |> Map.ofSeq
       Nodes = Map.keys m
       Edges = edges |}

let combGraph =
    let g = getGraph keypadString

    let nodes =
        g.Nodes
        |> Seq.collect (fun n -> dirToChar.Values |> Seq.map (fun m -> n, m))
        |> Seq.toList

    let neighF p =
        let moves =
            g.Edges
            |> List.filter (fun (u, v) -> u = p)
            |> List.map (fun (_, v) -> let c2 = dirToChar[posMinus v p] in v, string c2)

        moves

    {| Nodes = nodes; NeighF = neighF |}

let dijkstra (initNodes: ('n * int) list) (neighF: 'n -> ('n * _) list) costF targets resultMap =
    let pq = System.Collections.Generic.PriorityQueue<'n * string * 'n list, int>()
    let visited = System.Collections.Generic.HashSet<_>()
    let paths = System.Collections.Generic.Dictionary<_, _ list>()
    let mutable opt = None

    let dequeue () =
        let (success, n, p) = pq.TryDequeue()

        if success then
            //if visited.Count % 1000 = 0 then printfn "%A" (node, p)
            Some(n, p)
        else
            None

    initNodes |> Seq.iter (fun (n, p) -> pq.Enqueue((n, "", targets), p))

    let rec step () =
        match dequeue (), opt with
        | Some(node, _), _ when visited.Contains(node) ->
            //printfn $"Already visited: %A{node}, %A{p}"
            step ()
        | None, _ -> None
        //| Some(node, p), Some o when finishCond node && p > o -> result p |> resultMap
        | Some((node, s, []), p), None ->
            printfn $"FINISH %A{node} %s{s} %A{p}"
            opt <- Some p
            Some(resultMap (p, s))
        | Some((node, s, t :: ts), p), None when t = node ->
            //printfn $"Target %A{node} %s{s} %A{p}"
            pq.Enqueue((node, s+"A", ts), p)
            step()
        | Some(((n, s, ts) as node), p), _ ->
            //printfn $"%A{node}, %A{p}"
            visited.Add(node) |> ignore

            neighF n
            |> Seq.iter (fun (n, s') ->
                let c = costF (s + s')
                pq.Enqueue((n, s + s', ts), c))

            step ()
        | None, None -> failwith "No solution"

    step ()

let solve n start numbers =
    let g = getGraph keypadString
    let targets = numbers |> List.map (fun number -> g.Map[number])

    let neighF p =
        combGraph.NeighF p |> List.map (fun (n, c) -> n, c)

    let (cost, s) =
        dijkstra
            [ g.Map[start], 0 ]
            neighF
            (fun (s: string) -> ((s) |> layerN n))
            targets
            id
        |> Option.get
    //let cost = cost + (dirPadMoves (d, 'A') |> _.Length)
    printfn "%A" (start, numbers, s, cost)
    cost, s

let part1 lines =
    lines
    |> Seq.map (fun (s: string) ->
        let (_, p) = solve 2 'A' (Seq.toList s)
        let l1 = p |> oneLayer
        let full = p |> oneLayer |> oneLayer
        let cost = full |> _.Length
        let cost2 = layerN 2 p
        printfn "%i %i %A %A" cost cost2 s p
        printfn "%A" l1
        printfn "%A" full
        let x = s[.. s.Length - 2] |> int
        cost * x)
    |> Seq.toList

let part2 lines =
    lines
    |> Seq.map (fun (s: string) ->
        let (_, p) = solve 25 'A' (Seq.toList s)
        let l1 = p |> oneLayer
        let cost = p |> layerN 25
        printfn "%i %A %A" cost s p
        printfn "%A" l1
        printfn "%A" cost
        let x = s[.. s.Length - 2] |> int
        cost * x)
    |> Seq.toList

let sol =
    { Day = 21
      Part1 = solution part1 (Seq.sum >> string)
      Part2 = solution part2 (Seq.length >> string) }
