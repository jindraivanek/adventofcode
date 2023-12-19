#time

//let lines = System.IO.File.ReadAllLines("input")
let lines = System.IO.File.ReadAllLines("sample")

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

module List =
    let splitWhen (pred: 'a -> bool) (xs: 'a list) =
        let rec loop acc xs =
            match xs with
            | [] -> [List.rev acc]
            | x::xs ->
                if pred x then
                    List.rev acc :: loop [] xs
                else
                    loop (x::acc) xs
        loop [] xs


let workflows, parts =
    let [lines1; lines2] = lines |> Seq.toList |> List.splitWhen (fun s -> s = "")
    let workflows =
        lines1
        |> Seq.map (function
            | Match "(.*)\{(.*)\}" [ name; deps ] -> 
                name, 
                deps.Split ","
                |> Seq.map(function
                    | Match "(.)>(.*):(.*)" [ stat; value; target ] -> let x = int value in target, Some (stat, x, '>')
                    | Match "(.)<(.*):(.*)" [ stat; value; target ] -> let x = int value in target, Some (stat, x, '<')
                    | target -> target, None
                    )
                |> Seq.toList)
        |> Map.ofSeq
    let parts =
        lines2
        |> Seq.map (function
            | Match "\{(.*)\}" [ part ] -> 
                part.Split ","
                |> Seq.map(function
                    | Match "(.)=(.*)" [ stat; value ] -> stat, int value)
                |> Map.ofSeq)
        |> Seq.toList
    workflows, parts

let rec partAccepted part wName =
    let w = Map.find wName workflows
    match w |> List.find (fun (_, o) -> o |> Option.forall (fun (s, x, op) -> 
        let f = match op with | '<' -> (<) | '>' -> (>) | _ -> failwith "op"
        f (Map.find s part) x)) |> fst with
    | "A" -> true
    | "R" -> false
    | wName -> partAccepted part wName

let limitRanges r = function
    | None -> r
    | Some (s, x, op) -> 
        let lb, ub = Map.find s r
        match op with
        | '<' -> r |> Map.add s (lb, min ub x)
        | '>' -> r |> Map.add s (max lb x, ub)

let rec acceptingRanges wName r =
    let w = Map.find wName workflows
    w |> List.collect (fun (t, o) ->
        match t with
        | "A" -> 
            let r = limitRanges r o
            printfn "%A" r
            [r]
        | "R" -> []
        | t -> acceptingRanges t (limitRanges r o)
    )

let combineRanges r1 r2 =
    if r1 <> r2 && Map.forall (fun k (lb1, ub1) -> Map.find k r2 |> fun (lb2, ub2) -> (lb1 <= lb2 && lb2 <= ub1) || (lb2 <= lb1 && lb1 <= ub2)) r1 then
        r1 |> Map.map (fun k (lb1, ub2) -> let lb, ub = Map.find k r2 in (min lb1 lb), (max ub2 ub))|> Some
    else None

let rec combineRangesList xs =
    match xs |> List.tryPick (fun r1 -> xs |> List.tryPick (fun r2 -> combineRanges r1 r2)) with
    | Some 

let startRanges = "xmas" |> Seq.map (fun c -> string c, (0, 4001)) |> Map.ofSeq
let rangesCombination = startRanges |> acceptingRanges "in" |> combineRangesList
printfn "%A" rangesCombination

let part1 = parts |> List.filter (fun part -> partAccepted part "in") |> List.sumBy (Map.values >> Seq.sum)
let part2 = 
    rangesCombination
    |> Seq.sumBy (Map.values >> Seq.map (fun (lb, ub) -> ub - lb - 1 |> int64) >> Seq.reduce (*))

printfn $"{part1}" // 287054
printfn $"{part2}"
