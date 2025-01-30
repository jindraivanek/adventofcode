module day05

open Common

let parseInput (lines: string[]) =
    let lines1 = lines |> Seq.takeWhile (fun s -> s <> "")
    let lines2 = lines |> Seq.skipWhile (fun s -> s <> "") |> Seq.skip 1

    let ordering =
        lines1
        |> Seq.map (fun s -> s.Split('|') |> Seq.map int |> Seq.toList |> (fun xs -> xs[0], xs[1]))
        |> Seq.toList

    let updates =
        lines2
        |> Seq.map (fun s -> s.Split(',') |> Seq.map int |> Seq.toList)
        |> Seq.toArray

    ordering, updates

let part1 lines =
    let ordering, updates = parseInput lines

    updates
    |> Seq.filter (fun u ->
        let indexes = u |> List.mapi (fun i x -> x, i) |> Map.ofList

        ordering
        |> List.forall (fun (a, b) ->
            match Map.tryFind a indexes, Map.tryFind b indexes with
            | Some a, Some b -> a < b
            | _ -> true))

let part2 lines =
    let ordering, updates = parseInput lines

    let cmpMap =
        ordering |> List.collect (fun (a, b) -> [ (a, b), -1; (b, a), 1 ]) |> Map.ofList

    updates
    |> Seq.choose (fun u ->
        let xs = u |> List.sortWith (fun a b -> cmpMap[(a, b)])
        if u = xs then None else Some xs)

let sol =
    { Day = 5
      Part1 = solution part1 (Seq.map (fun xs -> xs[xs.Length / 2]) >> Seq.sum >> string)
      Part2 = solution part2 (Seq.map (fun xs -> xs[xs.Length / 2]) >> Seq.sum >> string) }
