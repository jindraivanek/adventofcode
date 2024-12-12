module day12
open Common
open Common.Grid

let parseInput (lines: string[]) =
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> (i, j), c)) |> Seq.concat |> Map.ofSeq
    {| Map = m; Size = lines.[0].Length, lines.Length |}

let floodFill start (m: Map<_,_>) =
    let field = m[start]
    let rec recF acc q =
        match q with
        | [] -> acc
        | p :: q when Map.tryFind p m = Some field && not (Set.contains p acc) -> recF (Set.add p acc) ((dirs |> List.map (posPlus p)) @ q)
        | _ :: q -> recF acc q

    recF Set.empty [ start ]
    
let areas m =
    (Map.empty, (m |> Map.keys)) ||> Seq.fold (fun acc p ->
        match Map.tryFind m[p] acc with
        | Some s when s |> List.exists (Set.contains p) -> acc
        | None -> Map.add m[p] [(floodFill p m)] acc
        | Some s -> Map.add m[p] ((floodFill p m) :: s) acc)
    
let edges m =
    m |> Map.keys |> Seq.collect (fun p -> dirs |> List.map (fun d -> p, posPlus p d))
    |> Seq.choose (fun (p, p2) -> if Map.containsKey p2 m then (if m[p] <> m[p2] then Some (p, Some p2) else None) else Some (p, None))
    |> Seq.toList

let part1 lines = 
   let s = parseInput lines
   let areas = areas s.Map
   let areaSizes = areas |> Map.toSeq |> Seq.collect (snd >> Seq.collect (fun s -> let c = Set.count s in s |> Seq.map (fun x -> x, c))) |> Map.ofSeq
   let edges = edges s.Map
   edges |> Seq.sumBy (fun (p, _) -> if Map.containsKey p areaSizes then areaSizes[p] else 0)

let part2 lines = 
    parseInput lines

let sol = {
    Day = 12
    Part1 = solution part1 (string)
    Part2 = solution part2 (_.Map >> Seq.length >> string)
}
