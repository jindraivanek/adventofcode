module day23
open Common

let parseInput (lines: string[]) =
    let edges = lines |> Seq.map (fun s -> s.Split('-') |> fun xs -> xs[0], xs[1]) |> Seq.toList |> List.collect (fun (u, v) -> [u, v; v, u])
    let nodes = edges |> List.collect (fun (u, v) -> [u; v]) |> Set.ofList
    //let neighMap = edges |> List.collect (fun (u, v) -> [u, v; v, u]) |> List.groupBy fst |> List.map (fun (u, vs) -> u, Set.ofList vs) |> Map.ofList
    {| Nodes = nodes; Edges = set edges |}

let part1 lines = 
   let g = parseInput lines
   let tNodes = g.Nodes |> Seq.filter (fun v ->v.StartsWith "t")   
   let triples = 
    tNodes |> Seq.collect (fun u -> g.Nodes |> Seq.collect (fun v -> g.Nodes |> Seq.map (fun w -> set [u; v; w]))) |> Seq.filter (fun s -> Set.count s = 3) |> set
     
   let r = triples |> Seq.filter (fun s -> Seq.allPairs s s |> Seq.forall (fun (u, v) -> u=v || g.Edges.Contains(u, v)))
   r |> Seq.iter (printfn "%A")
   r

let part2 lines = 
    parseInput lines
    [0]

let sol = {
    Day = 23
    Part1 = solution part1 (Seq.length >> string) //243 too low, 1850 too high
    Part2 = solution part2 (Seq.length >> string)
}
