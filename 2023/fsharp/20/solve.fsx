#time

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/20.txt")
//let lines = System.IO.File.ReadAllLines("sample")
//let lines = System.IO.File.ReadAllLines("sample2")

type Signal =
    | Low
    | High

type Node =
    | Broadcast
    | FlipFlop of bool
    | Conjuction of Map<string, Signal>
    | Output of Signal

let parsed =
    lines
    |> Seq.map (fun line ->
        let [| name; s |] = line.Split(" -> ")
        let nodeType = name[0]
        let name = if nodeType <> 'b' then name.Substring(1) else name
        let dests = s.Split(", ") |> Seq.toList
        name, nodeType, dests)
    |> Seq.toList

let destsMap =
    parsed |> List.map (fun (name, _, dests) -> name, dests) |> Map.ofList

let sourcesMap =
    parsed
    |> Seq.collect (fun (name, _, dests) -> dests |> Seq.map (fun dest -> dest, name))
    |> Seq.toList
    |> List.groupBy fst
    |> Map.ofList
    |> Map.map (fun _ -> List.map snd)

let init =
    parsed
    |> Seq.map (fun (name, nodeType, dests) ->
        let node =
            match nodeType with
            | 'b' -> Broadcast
            | '%' -> FlipFlop false
            | '&' -> Conjuction(sourcesMap |> Map.find name |> List.map (fun s -> s, Low) |> Map.ofList)
            | _ -> failwith "nodeType"

        name, node)
    |> Map.ofSeq

let nodeApplySignal node signal source =
    match node, signal with
    | Broadcast, _ -> Broadcast, Some signal
    | FlipFlop x, Low -> FlipFlop(not x), Some(if x then Low else High)
    | FlipFlop x, High -> FlipFlop x, None
    | Conjuction m, _ ->
        let m = m |> Map.add source signal

        if m |> Map.forall (fun _ s -> s = High) then
            Conjuction m, Some Low
        else
            Conjuction m, Some High
    | Output _, _ -> Output signal, None

let simOneSignal n (conf, cycleDetect) signal source dest =
    //printfn $"{source} -> {dest} {signal}"
    match Map.tryFind dest conf with
    | None -> (Map.add dest (Output signal) conf, cycleDetect), []
    | Some node ->
        let node, signal = nodeApplySignal node signal source
        let c = Map.add dest node conf

        let signals =
            match signal with
            | Some signal -> destsMap |> Map.find dest |> List.map (fun t -> dest, t, signal)
            | None -> []

        let cd =
            signal
            |> Option.map (fun signal ->
                let last = cycleDetect |> Map.tryFind (dest, signal) |> Option.defaultValue 0L
                let m = n - last
                cycleDetect |> Map.add (dest, signal) (m + 1L))
            |> Option.defaultValue cycleDetect

        (c, cd), signals

let rec simPhase n conf (lowCount, highCount) signals =
    match signals with
    | [] -> conf, (lowCount, highCount)
    | _ ->
        let c, newSignals =
            ((conf, []), signals)
            ||> List.fold (fun (c, acc) (dest, target, signal) ->
                let c2, xs = simOneSignal n c signal dest target
                c2, xs @ acc)

        let signalsCount = signals |> List.countBy (fun (_, _, x) -> x) |> Map.ofList
        let lowCount = lowCount + (signalsCount |> Map.tryFind Low |> Option.defaultValue 0)

        let highCount =
            highCount + (signalsCount |> Map.tryFind High |> Option.defaultValue 0)

        simPhase n c (lowCount, highCount) newSignals

let pressButton n c =
    simPhase n c (0, 0) [ "button", "broadcaster", Low ]

let rec pressButtonRepeat endResult c n (lo, hi) =
    match endResult n c (lo, hi) with
    | Some x -> x
    | None ->
        let c, (lo1, hi1) = pressButton n c
        pressButtonRepeat endResult c (n + 1L) (lo + lo1, hi + hi1)

let pressButtonN c n (lo, hi) =
    pressButtonRepeat (fun x _ lohi -> if x = n then Some lohi else None) c 0 (lo, hi)

let part1 () =
    pressButtonN (init, Map.empty) 1000 (0, 0)
    |> fun (lo, hi) -> int64 lo * int64 hi

let rec nodesFromEnd n xs =
    if n = 0 then
        xs
    else
        let xs = xs |> List.collect (fun name -> sourcesMap |> Map.find name)
        nodesFromEnd (n - 1) xs

let detectCycleIn = nodesFromEnd 2 [ "rx" ]

let endResult2 =
    fun n (c, cd) _ ->
        let cycles = detectCycleIn |> List.choose (fun name -> Map.tryFind (name, High) cd)

        if List.length cycles = List.length detectCycleIn then
            Some cycles
        else
            None

let part2 () =
    pressButtonRepeat endResult2 (init, Map.empty) 0L (0, 0)
    |> List.map int64
    |> List.reduce (*)

printfn $"{part1 ()}"
printfn $"{part2 ()}"
