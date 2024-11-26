module day17

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2023/17.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let teePrint x =
    printfn "%A" x
    x

let colorPrint x =
    let c = System.ConsoleColor.Green
    let old = System.Console.ForegroundColor
    System.Console.ForegroundColor <- c
    printf "%i" x
    System.Console.ForegroundColor <- old

let printOptimalPath (set: Set<(int * int)>) costMap =
    let minX = set |> Set.map fst |> Seq.min
    let maxX = set |> Set.map fst |> Seq.max
    let minY = set |> Set.map snd |> Seq.min
    let maxY = set |> Set.map snd |> Seq.max

    for x in minX..maxX do
        for y in minY..maxY do
            if Set.contains (x, y) set then
                colorPrint (Map.find (x, y) costMap)
            else
                printf "%i" (Map.find (x, y) costMap)

        printfn ""

let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let posMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let dirNeg (x, y) = (-x, -y)
let dirs = [ (0, -1); (-1, 0); (0, 1); (1, 0) ]

let pathStraightNum path =
    match List.pairwise path |> List.map (fun (x, y) -> posMinus x y) with
    | d :: _ as xs -> xs |> List.takeWhile (fun d' -> d' = d) |> List.length
    | _ -> 0

let dijkstra (initNodes: ('n * int) list) (neighF: 'n list -> ('n * int) list) (finishCond: 'n -> bool) =
    let pq = System.Collections.Generic.PriorityQueue<'n list, int>()
    let visited = System.Collections.Generic.HashSet<_>()

    let dequeue () =
        let (success, path, p) = pq.TryDequeue()

        if success then
            //if visited.Count % 1000 = 0 then printfn "%A" (node, p)
            Some(path, p)
        else
            None

    initNodes |> Seq.iter (fun (n, p) -> pq.Enqueue([ n ], p))

    let prevNode path = path |> List.tail |> List.tryHead

    let rec step () =
        match dequeue () with
        | Some((node :: _ as path), p) when visited.Contains(node, pathStraightNum path, prevNode path) -> step ()
        | Some((node :: _ as path), p) when finishCond node -> Some(path, p)
        | None -> None
        | Some((node :: _ as path), p) ->
            visited.Add(node, pathStraightNum path, prevNode path)

            neighF path
            |> Seq.iter (fun (n, p') ->
                //printfn $"%A{(n::path, (pathStraightNum (n::path)), p+p')}"
                let s = pathStraightNum (n :: path)
                pq.Enqueue(n :: path, p + p'))

            if finishCond node then
                printfn $"%A{(path, p)}"

            step ()

    step ()

let costs =
    lines
    |> Seq.mapi (fun y s -> s |> Seq.mapi (fun x c -> (x, y), c |> string |> int))
    |> Seq.collect id
    |> Map.ofSeq

let neigh minStraight maxStraight path =
    let allowedDirs =
        match path with
        | n1 :: n2 :: _ as path ->
            let d = posMinus n1 n2

            if pathStraightNum path >= maxStraight then
                dirs |> List.filter (fun d' -> d' <> d && d' <> dirNeg d)
            elif pathStraightNum path < minStraight then
                [ d ]
            else
                dirs |> List.filter (fun d' -> d' <> dirNeg d)
        | _ -> dirs

    match path with
    | n :: _ ->
        allowedDirs
        |> List.map (fun d -> posPlus n d)
        |> List.choose (fun n -> costs |> Map.tryFind n |> Option.map (fun p -> n, p))
    | _ -> failwith "neigh"

let target = costs |> Map.keys |> Seq.max
let start = [ (0, 0), 0 ]

let optimalPathCost minStraight maxStraight =
    let finishCond n = n = target
    let r = dijkstra start (neigh minStraight maxStraight) finishCond
    let path = r |> Option.map (fst >> List.rev) |> Option.defaultValue []
    //printOptimalPath (path |> Set.ofList) costs
    r |> Option.map snd

let part1 = optimalPathCost 0 3 // 847
let part2 = optimalPathCost 4 10 //997

printfn $"{part1}"
printfn $"{part2}"
