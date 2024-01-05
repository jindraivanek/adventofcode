#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

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
            | [] -> [ List.rev acc ]
            | x :: xs ->
                if pred x then
                    List.rev acc :: loop [] xs
                else
                    loop (x :: acc) xs

        loop [] xs


let workflows, parts =
    let [ lines1; lines2 ] = lines |> Seq.toList |> List.splitWhen (fun s -> s = "")

    let workflows =
        lines1
        |> Seq.map (function
            | Match "(.*)\{(.*)\}" [ name; deps ] ->
                name,
                deps.Split ","
                |> Seq.map (function
                    | Match "(.)>(.*):(.*)" [ stat; value; target ] -> let x = int value in target, Some(stat, x, '>')
                    | Match "(.)<(.*):(.*)" [ stat; value; target ] -> let x = int value in target, Some(stat, x, '<')
                    | target -> target, None)
                |> Seq.toList)
        |> Map.ofSeq

    let parts =
        lines2
        |> Seq.map (function
            | Match "\{(.*)\}" [ part ] ->
                part.Split ","
                |> Seq.map (function
                    | Match "(.)=(.*)" [ stat; value ] -> stat, int value)
                |> Map.ofSeq)
        |> Seq.toList

    workflows, parts

let rec partAccepted part wName =
    let w = Map.find wName workflows

    match
        w
        |> List.find (fun (_, o) ->
            o
            |> Option.forall (fun (s, x, op) ->
                let f =
                    match op with
                    | '<' -> (<)
                    | '>' -> (>)
                    | _ -> failwith "op"

                f (Map.find s part) x))
        |> fst
    with
    | "A" -> true
    | "R" -> false
    | wName -> partAccepted part wName

let limitRanges r =
    function
    | None -> r
    | Some(s, x, op) ->
        let lb, ub = Map.find s r

        match op with
        | '<' -> r |> Map.add s (lb, min ub x)
        | '>' -> r |> Map.add s (max lb x, ub)

let reverseOp =
    function
    | None -> None
    | Some(s, x, op) ->
        match op with
        | '<' -> s, x - 1, '>'
        | '>' -> s, x + 1, '<'
        |> Some

let rec acceptingRanges wNames r =
    let w = Map.find (List.head wNames) workflows

    w
    |> List.fold
        (fun (s, acc) (t, o) ->
            let cont = limitRanges s (reverseOp o)

            match t with
            | "A" ->
                let r = limitRanges s o
                //printfn "%A %A" wNames r
                cont, r :: acc
            | "R" -> cont, acc
            | t -> cont, acc @ acceptingRanges (t :: wNames) (limitRanges s o))
        (r, [])
    |> snd

let intersectRanges (lb1, ub1) (lb2, ub2) =
    let lb = max lb1 lb2
    let ub = min ub1 ub2
    if lb <= ub then Some(lb, ub) else None

let rangeMapSize m =
    m
    |> Map.values
    |> Seq.map (fun (lb, ub) -> ub - lb - 1 |> int64)
    |> Seq.reduce (*)

let intersectRangeMaps m1 m2 =
    if
        m1 <> m2
        && Map.forall (fun k r1 -> Map.find k m2 |> fun r2 -> (intersectRanges r1 r2).IsSome) m1
    then
        let r =
            m1
            |> Map.map (fun k r1 -> let r2 = Map.find k m2 in intersectRanges r1 r2 |> Option.get)

        Some r
    else
        None

let pairWith xs ys f =
    xs
    |> Seq.collect (fun x ->
        ys
        |> Seq.filter (snd >> (<>) x)
        |> Seq.choose (fun (s, y) ->
            if Set.contains x s then
                None
            else
                f x y |> Option.map (fun z -> Set.add x s, z)))

let rec rangeMapsSize size acc xs =
    //printfn "SIZE %A" size
    let label x = xs |> List.findIndex ((=) x)

    if size = 1 then
        let singlesSize = xs |> Seq.sumBy rangeMapSize
        //printfn "%A" singlesSize
        singlesSize + rangeMapsSize 2 (xs |> List.map (fun x -> set [ x ], x)) xs
    else
        let pairsIntersects =
            pairWith xs acc intersectRangeMaps |> Seq.distinctBy fst |> Seq.toList

        let sign = if size % 2 = 1 then 1L else -1L

        if pairsIntersects.IsEmpty then
            0L
        else
            let x = (pairsIntersects |> Seq.sumBy (snd >> rangeMapSize)) * sign
            //printfn "%A %A" x pairsIntersects.Length
            //pairsIntersects |> Seq.iter (fun (s, m) -> printfn "[%A] %A %A" (s |> Set.map label) m (rangeMapSize m))
            x + rangeMapsSize (size + 1) pairsIntersects xs

let startRanges = "xmas" |> Seq.map (fun c -> string c, (0, 4001)) |> Map.ofSeq
let rangesCombination = startRanges |> acceptingRanges [ "in" ] |> List.distinct
//printfn "%A" (List.indexed rangesCombination)

let part1 =
    parts
    |> List.filter (fun part -> partAccepted part "in")
    |> List.sumBy (Map.values >> Seq.sum)

let part2 = rangesCombination |> rangeMapsSize 1 []

printfn $"{part1}" // 287054
printfn $"{part2}" // 131619440296497
