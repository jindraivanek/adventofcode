#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")
//let lines = System.IO.File.ReadAllLines("sample2")

let important = set["hb";  "bs"; "js"; "rr"; "zn";  "gr"; "st"; "lg"; "bn"]//;  "hm"; "jv"; "pc"; "vq"]

type Signal =
    | Low
    | High

type Node =
    | Broadcast
    | FlipFlop of bool
    | Conjuction of Map<string, Signal>
    | Output of Signal

let parsed =
    lines |> Seq.map (fun line ->
        let [| name; s |] = line.Split(" -> ")
        let nodeType = name[0]
        let name = if nodeType <> 'b' then name.Substring(1) else name
        let dests = s.Split(", ") |> Seq.toList
        name, nodeType, dests)
    |> Seq.toList

let destsMap =
    parsed |> List.map (fun (name, _, dests) -> name, dests) |> Map.ofList

let sourcesMap =
    parsed |> Seq.collect (fun (name, _, dests) -> dests |> Seq.map (fun dest -> dest, name)) 
    |> Seq.toList |> List.groupBy fst |> Map.ofList |> Map.map (fun _ -> List.map snd)

let init =
    parsed |> Seq.map (fun (name, nodeType, dests) ->
        let node =
            match nodeType with
            | 'b' -> Broadcast
            | '%' -> FlipFlop false
            | '&' -> Conjuction (sourcesMap |> Map.find name |> List.map (fun s -> s, Low) |> Map.ofList)
            | _ -> failwith "nodeType"
        name, node)
    |> Map.ofSeq

let nodeApplySignal node signal source =
    match node, signal with
    | Broadcast, _ -> Broadcast, Some signal
    | FlipFlop x, Low -> FlipFlop(not x), Some (if x then Low else High)
    | FlipFlop x, High -> FlipFlop x, None
    | Conjuction m, _ ->
        let m = m |> Map.add source signal

        if m |> Map.forall (fun _ s -> s = High) then
            Conjuction m, Some Low
        else
            Conjuction m, Some High
    | Output _, _ -> Output signal, None

let simOneSignal conf signal source dest =
    //printfn $"{source} -> {dest} {signal}"
    match Map.tryFind dest conf with
    | None -> Map.add dest (Output signal) conf, []
    | Some node ->
        let node, signal = nodeApplySignal node signal source
        let c = Map.add dest node conf
        let signals = 
            match signal with
            | Some signal -> destsMap |> Map.find dest |> List.map (fun t -> dest, t, signal)
            | None -> []
        c, signals

let rec simPhase conf (lowCount, highCount) signals =
     match signals with
     | [] -> conf, (lowCount, highCount)
     | _ ->
        let c, newSignals = 
            ((conf, []), signals) ||> List.fold (fun (c, acc) (dest, target, signal) -> 
                let c2, xs = simOneSignal c signal dest target
                c2, xs @ acc)
        let signalsCount = signals |> List.countBy (fun (_,_,x) -> x) |> Map.ofList
        let lowCount = lowCount + (signalsCount |> Map.tryFind Low |> Option.defaultValue 0)
        let highCount = highCount + (signalsCount |> Map.tryFind High |> Option.defaultValue 0)
        simPhase c (lowCount, highCount) newSignals

let pressButton c =
    simPhase c (0, 0) [ "button", "broadcaster", Low ]

let rec pressButtonRepeat endCond c n (lo, hi) =
    if n%1_000_000L = 0L then printfn "%A %i" (System.DateTime.Now) n
    if endCond n c then
        n, (lo, hi)
    else
        let c, (lo1, hi1) = pressButton c
        pressButtonRepeat endCond c (n + 1L) (lo + lo1, hi + hi1)

let pressButtonN c n (lo, hi) = pressButtonRepeat (fun x _ -> x = n) c 0 (lo, hi) |> snd

printfn "%A" (pressButton init)

let part1 () = pressButtonN init 1000 (0, 0) |> fun (lo, hi) -> int64 lo * int64 hi
let endCond = 
    let mutable seenNodes = set[]
    let mutable prevC = init
    let mutable lastChange = Map.empty
    let isChange a b = 
        let allHigh m = m |> Map.forall (fun k v -> v = High)
        match a,b with
        | FlipFlop a, FlipFlop b -> a <> b
        | Conjuction a, Conjuction b -> allHigh a <> allHigh b 
        | _ -> false
    fun n c ->
        Map.keys c |> Seq.filter (fun k -> (Set.contains k important && isChange prevC[k] c[k]) || (not (Set.contains k seenNodes) && Some c[k] <> Map.tryFind k init)) 
        |> Seq.iter (fun k -> 
            printfn "change %i %i %A %A" (n-(lastChange |> Map.tryFind k |> Option.defaultValue 0L)) n k (c |> Map.find k)
            lastChange <- lastChange |> Map.add k n
            seenNodes <- Set.add k seenNodes
            //let rem = c |> Map.filter (fun k _ -> not (Set.contains k seenNodes))
            //printfn $"%A{rem}"
            )
        prevC <- c
        match Map.tryFind "rx" c with
        | Some (Output Low) -> true
        | _ -> false
let part2 () = pressButtonRepeat endCond init 0 (0, 0) |> snd |> fun (lo, hi) -> int64 lo * int64 hi

printfn $"{part1 ()}" //684125385
printfn $"{part2 ()}"
