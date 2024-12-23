module day23

open Common

let parseInput (lines: string[]) =
    let edges =
        lines
        |> Seq.map (fun s -> s.Split('-') |> fun xs -> xs[0], xs[1])
        |> Seq.toList
        |> List.collect (fun (u, v) -> [ u, v; v, u ])

    let nodes = edges |> List.collect (fun (u, v) -> [ u; v ]) |> Set.ofList

    let neighMap =
        edges
        |> List.collect (fun (u, v) -> [ u, v; v, u ])
        |> List.groupBy fst
        |> List.map (fun (u, vs) -> u, vs |> List.map snd |> set)
        |> Map.ofList

    {| Nodes = nodes
       Edges = set edges
       NeighMap = neighMap |}

let part1 lines =
    let g = parseInput lines
    let tNodes = g.Nodes |> Seq.filter (fun v -> v.StartsWith "t")

    let triples =
        tNodes
        |> Seq.collect (fun u -> g.Nodes |> Seq.collect (fun v -> g.Nodes |> Seq.map (fun w -> set [ u; v; w ])))
        |> Seq.filter (fun s -> Set.count s = 3)
        |> set

    let r =
        triples
        |> Seq.filter (fun s -> Seq.allPairs s s |> Seq.forall (fun (u, v) -> u = v || g.Edges.Contains(u, v)))
    //r |> Seq.iter (printfn "%A")
    r

let part2 lines =
    let g = parseInput lines
    let isClique c n = Set.isSubset c g.NeighMap[n]

    let rec maxClique c =
        function
        | [] -> c
        | n :: ns ->
            if isClique c n then
                maxClique (Set.add n c) ns
            else
                maxClique c ns

    let cliques =
        g.Edges
        |> Seq.map (fun (u, v) ->
            let n = Set.intersect g.NeighMap[u] g.NeighMap[v]
            //printfn "%s-%s %i" u v n.Count
            let c =
                maxClique
                    (set[u
                         v])
                    (Set.toList n)

            c)
        |> Seq.sortByDescending (fun c -> c.Count)
        |> Seq.toList
    //cliques|> Seq.iter (fun c -> printfn "%i %A" c.Count c)
    cliques |> Seq.head |> String.concat ","

let sol =
    { Day = 23
      Part1 = solution part1 (Seq.length >> string)
      Part2 = solution part2 id }
