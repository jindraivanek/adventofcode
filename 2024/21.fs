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

let dirPadMovesMulti =
    function
    | 'A', 'A' -> ["A"]
    | 'A', '^' -> ["<A"]
    | 'A', '>' -> ["vA"]
    | 'A', 'v' -> ["v<A"; "<vA"]
    | 'A', '<' -> ["v<<A"; "<v<A"]
    | '^', 'A' -> [">A"]
    | '^', '^' -> ["A"]
    | '^', '>' -> ["v>A"; ">vA"]
    | '^', 'v' -> ["vA"]
    | '^', '<' -> ["v<A"]
    | '>', 'A' -> ["^A"]
    | '>', '^' -> ["^<A"; "<^A"]
    | '>', '>' -> ["A"]
    | '>', 'v' -> ["<A"]
    | '>', '<' -> ["<<A"]
    | 'v', 'A' -> ["^>A"; ">^A"]
    | 'v', '^' -> ["^A"]
    | 'v', '>' -> [">A"]
    | 'v', 'v' -> ["A"]
    | 'v', '<' -> ["<A"]
    | '<', 'A' -> [">>^A"; ">^>A"]
    | '<', '^' -> [">^A"]
    | '<', '>' -> [">>A"]
    | '<', 'v' -> [">A"]
    | '<', '<' -> ["A"]
let dirPadMoves = dirPadMovesMulti >> List.head

let dirToChar = Seq.zip dirs "^>v<" |> Map.ofSeq
let dirOrder = [ '^'; '>'; 'v'; '<' ]
let dirs = dirs |> Seq.sortBy (fun d -> Seq.findIndex ((=) dirToChar[d]) dirOrder) |> Seq.toList

let oneLayer s =
    ("A" + s) |> Seq.pairwise |> Seq.map (dirPadMoves) |> String.concat ""
    
let rec oneLayerN n s = if n=0 then s else oneLayerN (n-1) (oneLayer s)

let pairLenMem = memoizeRec <| fun recF (n, a, b, start) ->
    let ss = dirPadMovesMulti (a, b)
    ss |> List.map (fun s ->
    if n = 1 || s.Length = 1 then int64 s.Length, s, Seq.last s
    else 
        let xs =
            string start + s |> Seq.pairwise 
            |> Seq.mapWithState (fun start (a,b) -> recF ((n-1), a, b, start) |> fun (x, s, e) -> e, (x, s, e)) start
            |> Seq.toList
        let last = xs |> List.last |> fun (_,_,c) -> c
        let s1 = xs |> Seq.sumBy (fun (a,_,_) -> a)
        printfn "%A -> %A" (a, b, n, start) s1
        s1, "", last
    ) |> List.minBy (fun (x, _, _) -> x)

let layerN n s = 
    ("A" + s) |> Seq.pairwise |> Seq.mapWithState (fun start (a,b) -> pairLenMem (n, a, b, start) |> fun (x, s, e) -> e, x) 'A' |> Seq.sum

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

let dijkstra (initNodes: ('n * int64) list) (neighF: 'n -> ('n * _) list) costF targets resultMap =
    let pq = System.Collections.Generic.PriorityQueue<'n * string * 'n list, int64>()
    let visited = System.Collections.Generic.HashSet<_>()
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
            step ()
        | None, _ -> None
        | Some((node, s, []), p), None ->
            printfn $"FINISH %A{node} %s{s} %A{p}"
            opt <- Some p
            Some(resultMap (p, s))
        | Some((node, s, t :: ts), p), None when t = node ->
            pq.Enqueue((node, s+"A", ts), costF (s + "A"))
            step()
        | Some(((n, s, ts) as node), p), _ ->
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
            (fun (s: string) -> (s |> layerN n))
            targets
            id
        |> Option.get
    printfn "%A" (start, numbers, s, cost)
    cost, s

let part1 lines =
    lines
    |> Seq.map (fun (s: string) ->
        let (pathCost, p) = solve 2 'A' (Seq.toList s)
        let l1 = p |> oneLayer
        let full = p |> oneLayerN 2
        let cost = full |> _.Length
        let cost2 = layerN 2 p
        printfn "%i %i %i %A %A" cost cost2 pathCost s p
        printfn "%A" l1
        printfn "%A" full
        let x = s[.. s.Length - 2] |> int
        cost * x)
    |> Seq.toList

let part2 lines =
    lines
    |> Seq.map (fun (s: string) ->
        let (pathCost, p) = solve 25 'A' (Seq.toList s)
        let l1 = p |> oneLayer
        let cost = p |> layerN 25
        printfn "%i %A %A" cost s p
        printfn "%A" l1
        printfn "%A" pathCost
        let x = s[.. s.Length - 2] |> int64
        printfn "%A" (cost * x)
        cost * x)
    |> Seq.toList

let sol =
    { Day = 21
      Part1 = solution part1 (Seq.sum >> string)
      Part2 = solution part2 (Seq.sum >> string)
       }
