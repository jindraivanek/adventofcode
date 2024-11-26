module day23

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2023/23.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let teePrint x =
    printfn "%A" x
    x

let grid =
    lines
    |> Seq.mapi (fun y s -> s |> Seq.mapi (fun x c -> (x, y), c))
    |> Seq.collect id
    |> Seq.filter (fun (_, c) -> c <> '#')
    |> Map.ofSeq

let dimY = lines.Length

let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let dirs = [ (0, -1); (-1, 0); (0, 1); (1, 0) ]

let dijkstra
    maxStepsOrEndOnFinishNode
    (initNodes: ('n * int) list)
    (neighF: 'n list -> ('n * int) list)
    (finishCond: 'n -> bool)
    failCond
    =
    let maxSteps =
        maxStepsOrEndOnFinishNode |> Option.defaultValue System.Int32.MaxValue

    let toEmpty = maxStepsOrEndOnFinishNode |> Option.isSome
    let startNodes = initNodes |> List.map fst |> set
    let pq = System.Collections.Generic.PriorityQueue<('n list * 'n Set), int>()
    let mutable bestFinish = [], System.Int32.MaxValue
    let mutable stepCount = 0

    let dequeue () =
        let (success, x, p) = pq.TryDequeue()

        if success then
            let ((n :: _ as path), tailSet) = x
            stepCount <- stepCount + 1
            Some(path, tailSet, p)
        else
            None

    initNodes |> Seq.iter (fun (n, p) -> pq.Enqueue(([ n ], Set.empty), p))

    let rec step () =
        if stepCount > maxSteps && snd bestFinish < System.Int32.MaxValue then
            Some(bestFinish)
        else
            match dequeue () with
            | Some(node :: _, tailSet, _) when Set.contains node tailSet -> step ()
            | None -> if toEmpty then Some(bestFinish) else None
            | Some((node :: _ as path), tailSet, p) ->
                if failCond node && not (Set.contains node startNodes) && not (finishCond node) then
                    step ()
                else
                    let s = Set.add node tailSet

                    neighF path
                    |> Seq.iter (fun (n, p') ->
                        if not (Set.contains n tailSet) then
                            pq.Enqueue((n :: path, s), p + p'))

                    if finishCond node then
                        if toEmpty then
                            if p < snd bestFinish then
                                //printfn $"BEST: %A{(path, p)}"
                                bestFinish <- path, p

                            step ()
                        else
                            Some(path, p)
                    else
                        step ()

    step ()

let neigh considerSlopes cost path =
    let allowedDirs =
        if not considerSlopes then
            dirs
        else
            match List.tryHead path |> Option.bind (fun n -> Map.tryFind n grid) with
            | Some '<' -> [ (-1, 0) ]
            | Some '>' -> [ (1, 0) ]
            | Some '^' -> [ (0, -1) ]
            | Some 'v' -> [ (0, 1) ]
            | Some '.' -> dirs

    match path with
    | n :: _ ->
        allowedDirs
        |> List.map (fun d -> posPlus n d)
        |> List.choose (fun n -> grid |> Map.tryFind n |> Option.map (fun _ -> n))
        |> List.map (fun m -> m, cost n m)

let crossroads neigh start =
    let rec step acc visited nodes =
        match nodes with
        | [] -> acc
        | n :: rest ->
            if Set.contains n visited then
                step acc visited rest
            else
                let visited' = Set.add n visited
                let neigh = neigh [ n ]
                step (if List.length neigh > 2 then n :: acc else acc) visited' ((neigh |> List.map fst) @ rest)

    step [] Set.empty [ start ]

let start = [ (1, 0), 0 ]
let endCond (_, y) = y = dimY - 1
let startNode = (1, 0)
let endNode = grid |> Map.findKey (fun k _ -> endCond k)

let cs =
    crossroads (neigh true (fun _ _ -> 1)) (1, 0)
    |> fun xs -> startNode :: endNode :: xs

let csSet = set cs - set [ startNode; endNode ]

let crossroadCosts =
    cs
    |> List.collect (fun n ->
        cs
        |> List.filter ((<>) n)
        |> List.choose (fun m ->
            dijkstra None [ n, 0 ] (neigh false (fun _ _ -> 1)) (fun x -> x = m) (fun x -> Set.contains x csSet)
            |> Option.map snd
            |> Option.map (fun x -> (n, m), x)))
    |> Map.ofSeq

let maxSteps = 10_000_000

let longestPath () =
    dijkstra (Some maxSteps) start (neigh true (fun _ _ -> -1)) endCond (fun _ -> false)

let crossroadNeigh path =
    match path with
    | n :: _ ->
        cs
        |> List.filter ((<>) n)
        |> List.choose (fun m -> Map.tryFind (n, m) crossroadCosts |> Option.map (fun x -> m, -x))
    | _ -> failwith "crossroadNeigh"

let longestCrossroadPath () =
    dijkstra (Some maxSteps) start crossroadNeigh endCond (fun _ -> false)

let part1 () =
    longestPath () |> Option.map (snd >> (~-))

let part2 () =
    longestCrossroadPath () |> Option.map (snd >> (~-))

printfn $"{part1 ()}"
printfn $"{part2 ()}"
