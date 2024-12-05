module day05
open Common

let parseInput (lines: string[]) =
    let lines1 =lines |> Seq.takeWhile (fun s -> s <> "")
    let lines2 =lines |> Seq.skipWhile (fun s -> s <> "") |> Seq.skip 1
    let ordering = lines1 |> Seq.map (fun s -> s.Split('|') |> Seq.map int |> Seq.toList |> fun xs -> xs[0], xs[1]) |> Seq.toList
    let updates = lines2 |> Seq.map (fun s -> s.Split(',') |> Seq.map int |> Seq.toList) |> Seq.toArray
    ordering, updates

let part1 lines = 
   let ordering, updates = parseInput lines
   updates
   |> Seq.filter (fun u -> 
       let indexes = u |> List.mapi (fun i x -> x, i) |> Map.ofList 
       ordering |> List.forall (fun (a, b) -> match Map.tryFind a indexes, Map.tryFind b indexes with | Some a, Some b -> a < b | _ -> true))

let part2 lines = 
   let swap i j (arr: _[]) = 
       printfn "%A -> %A" arr[j] arr[i]
       let tmp = arr[i]
       arr[i] <- arr[j]
       arr[j] <- tmp
   let ordering, updates = parseInput lines
   let orderIndex = ordering |> List.map snd |> List.countBy id |> Map.ofList
   let orderIndex2 = ordering |> List.map fst |> List.countBy id |> Map.ofList
   //let orderItems = ordering |> Seq.collect (fun (a,b) -> [a;b]) |> Seq.distinct |> Seq.sortBy (fun x -> orderIndex[x]) |> Seq.toList
   //printfn "%A" (ordering.Length, orderItems.Length)
   updates
   |> Seq.map (fun u -> 
       let u = u |> Seq.toArray
       let rec order() =
        if ordering |> List.exists (fun (a, b) -> match Array.tryFindIndex ((=)a) u, Array.tryFindIndex ((=)b) u with | Some i, Some j when i > j -> swap i j u; true | _ -> false) then order()
       printfn "%A" u
       order()
       printfn "%A" u
       if Seq.length u <> Seq.length (Seq.distinct u) then
        printfn "%A" u
       if ordering |> List.exists (fun (a, b) -> match Array.tryFindIndex ((=)a) u, Array.tryFindIndex ((=)b) u with | Some i, Some j when i > j -> true | _ -> false) then
        printfn "%A" u
       u |> Array.toList)

let sol = {
    Day = 5
    Part1 = solution part1 (Seq.map (fun xs -> xs[xs.Length / 2]) >> Seq.sum >> string)
    Part2 = solution part2 (Seq.map (fun xs -> xs[xs.Length / 2]) >> Seq.sum >> string)
}
