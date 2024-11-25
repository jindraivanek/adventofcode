#time

let lines = System.IO.File.ReadAllLines("__SOURCE_DIRECTORY__/../../../../input/2023/15.txt")
//let lines = System.IO.File.ReadAllLines("sample")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let strings = lines |> Seq.collect (fun s -> s.Split(',')) |> Seq.toList

let hash s =
    s
    |> Seq.map (fun c -> int c)
    |> Seq.fold (fun acc x -> ((acc + x) * 17) % 256) 0

let oneOp boxes s =
    match s with
    | Match "(.*)\-" [ label ] ->
        let boxIndex = hash label
        let b = boxes |> Map.tryFind boxIndex |> Option.defaultValue []
        let b = b |> List.filter (fun (l, _) -> l <> label)
        Map.add boxIndex b boxes
    | Match "(.*)\=([0-9]+)" [ label; x ] ->
        let boxIndex = hash label
        let b = boxes |> Map.tryFind boxIndex |> Option.defaultValue []
        let x = int x

        match b |> List.tryFind (fun (l, _) -> l = label) with
        | Some _ ->
            let b = b |> List.map (fun (l, y) -> if l = label then (l, x) else (l, y))
            Map.add boxIndex b boxes
        | None ->
            let b = (label, x) :: b
            Map.add boxIndex b boxes

let boxesScore boxes =
    boxes
    |> Map.map (fun i b -> b |> List.rev |> List.mapi (fun j (l, x) -> x * (i + 1) * (j + 1)) |> List.sum)
    |> Map.toList
    |> List.sumBy snd

let part1 = strings |> Seq.map hash |> Seq.sum

let part2 =
    let boxes = strings |> Seq.fold oneOp Map.empty
    //printfn $"%A{boxes}"
    boxes |> boxesScore

printfn $"{part1}"
printfn $"{part2}"
