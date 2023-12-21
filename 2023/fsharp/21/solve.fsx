#time

//let lines = System.IO.File.ReadAllLines("input")
let lines = System.IO.File.ReadAllLines("sample")

let memoize f =
    let cache = System.Collections.Generic.Dictionary<_, _>()

    fun x ->
        match cache.TryGetValue x with
        | true, v -> 
            //printfn "cache hit %A" (x,v)
            v
        | _ ->
            let v = f x
            cache.Add(x, v)
            v

let memoize2 f = memoize (fun (x, y) -> f x y) |> fun g -> fun x y -> g (x, y)

let printGrid s =
    let minX = s |> Seq.map fst |> Seq.min
    let maxX = s |> Seq.map fst |> Seq.max
    let minY = s |> Seq.map snd |> Seq.min
    let maxY = s |> Seq.map snd |> Seq.max
    printfn "%A" ((minX, minY), (maxX, maxY))

    for y in minY..maxY do
        for x in minX..maxX do
            if Set.contains (x, y) s then printf "#" else printf "."

        printfn ""

    printfn "----------------"

type SetVariant = int64

let fromSetVariant, toSetVariant =
    let mutable cache = Map.empty<(int * int) * Set<int64 * int64>, SetVariant>
    let mutable revCache = Map.empty
    let toSV s =
        match Map.tryFind s cache with
        | Some x -> x
        | None ->
            let x = Map.count cache |> int64 |> (+) 1L
            cache <- Map.add s x cache
            revCache <- Map.add x s revCache
            x
    let fromSV x = Map.find x revCache
    fromSV, toSV

let memSetVariantFromSeqFunc f =
    memoize <|
        fun (svs: Set<SetVariant>) ->
            let s = Seq.map fromSetVariant svs
            f s |> toSetVariant

let memSetVariantSeqFunc f =
    memoize <|
    fun sv ->
        let s = fromSetVariant sv
        f s |> Seq.map toSetVariant |> set

let inline mod_ a b = (a % b + b) % b

let inline posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let posMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let inline posMult (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
let posModulo (x1, y1) (x2, y2) = (mod_ x1 x2), (mod_ y1 y2)
let dirNeg (x, y) = (-x, -y)
let dirs = [ (0L, -1L); (-1L, 0L); (0L, 1L); (1L, 0L) ]

let dirs3x3 =
    [ (-1L, -1L)
      (0L, -1L)
      (1L, -1L)
      (-1L, 0L)
      (1L, 0L)
      (-1L, 1L)
      (0L, 1L)
      (1L, 1L) ]

let areaDirs3x3 = (0L, 0L) :: dirs3x3

let printGrids dim ss =
    ss |> Map.toSeq |> Seq.collect (fun ((dx, dy), s) -> s |> Seq.map (fun x -> posMult dim (int64 dx, int64 dy) |> posPlus x)) |> set |> printGrid

let indexed =
    lines
    |> Array.mapi (fun y line ->
        line
        |> Seq.mapi (fun x c -> (int64 x, int64 y), c)
        |> Seq.filter (fun (_, c) -> c <> '#'))
    |> Seq.concat
    |> Seq.toList

let dimX = int64 lines[0].Length
let dimY = int64 lines.Length
let inputDim = dimX, dimY
printfn $"{inputDim}"
let start = indexed |> Seq.find (fun (_, c) -> c = 'S') |> fst
let spaces = indexed |> Seq.map fst |> set

let spaces3x3 =
    areaDirs3x3
    |> Seq.collect (fun d -> spaces |> Seq.map (posPlus (posMult inputDim d)))
    |> set

let getArea d s =
    let x = spaces |> Seq.map (posPlus (posMult inputDim d)) |> set
    //printGrid x
    x

let areas3x3 = areaDirs3x3 |> Seq.map (fun d -> d, getArea d spaces3x3) |> Map.ofSeq

let isOutside (x, y) = x < 0L || y < 0L || x >= dimX || y >= dimY
let mirror (x, y) =
    if x < 0L then (-1, 0), (dimX + x, y)
    elif y < 0L then (0,-1),  (x, dimY + y)
    elif x >= dimX then (1, 0), (x - dimX, y)
    elif y >= dimY then (0, 1), (x, y - dimY)
    else (0, 0), (x, y)

let step spaces p =
    dirs |> Seq.map (posPlus p) |> Seq.filter (fun p -> Set.contains p spaces || isOutside p) |> Seq.map mirror

let allSteps = memSetVariantSeqFunc <| fun (_, s) -> 
    s |> Seq.collect (step spaces) |> Seq.groupBy fst |> Seq.map (fun (p, xs) -> p, xs |> Seq.map snd |> set)

let apply = memoize <| fun plots -> 
    plots |> Map.toSeq |> Seq.collect (fun (area, s) -> allSteps s |> Seq.map fromSetVariant |> Seq.map (fun (areaDir, x) -> posPlus area areaDir, x))
    |> Seq.groupBy fst |> Seq.map (fun (p, xs) -> p, toSetVariant (p, xs |> Seq.map snd |> Set.unionMany)) |> Map.ofSeq

let allStepsSeq _ plots =
    Seq.unfold (fun s -> Some(s, apply s)) plots

let allStepsN g s n =
    allStepsSeq g s
    |> Seq.indexed
    |> Seq.map (fun (i, x) ->
        if i%10 = 0 then printfn "%A" (i)
        x)
    |> Seq.nth n

let startPlots = Map.ofSeq [ (0,0), toSetVariant ((0,0), set [start]) ]

let startSeq spaces =
    allStepsSeq spaces startPlots |> Seq.cache
let countN spaces n =
    let m = allStepsN spaces startPlots n 
    //printGrids inputDim m
    m |> Map.values |> Seq.sumBy (fromSetVariant >> snd >> Set.count)

let part1 () = countN spaces 64
//printfn $"{part1()}"

//let seqIntoMax s = startSeq s |> Seq.indexed |> Seq.pairwise |> Seq.takeWhile (fun ((_,s1), (_,s2)) -> Set.isEmpty s2 || Set.count s1 < Set.count s2) |> Seq.toList |> fun xs -> (0, Seq.head (startSeq s)) :: (List.map snd xs)
// let seqIntoMax s =
//     let s = startSeq s |> Seq.cache

//     let xs =
//         s
//         |> Seq.scan
//             (fun (i, x, _) s ->
//                 printfn "%A" (i, x)
//                 let m = Set.count s in
//                 if m > x then (0, m, s) else (i + 1, x, s))
//             (0, 0, Set.empty)
//         |> Seq.takeWhile (fun (i, m, _) -> m = 0 || i <= 10)
//         |> Seq.skip 1
//         |> Seq.map (fun (_, _, s) -> s)

//     xs |> Seq.indexed |> Seq.toList

// let setCountsFiltered f s =
//     s
//     |> List.filter (fun (i, x) -> (*printfn "i%i %i" i x;*) f i)
//     |> List.map (snd >> Set.count)

// let distMap s =
//     s
//     |> Seq.pairwise
//     |> Seq.collect (fun ((_, x1), (i, x2)) -> x2 - x1 |> Seq.map (fun x -> x, i))
//     |> Map.ofSeq

// let maxInfo label area start s =
//     let s =
//         s
//         |> Seq.map (fun (i, x) -> i, Set.filter (fun p -> Set.contains p area) x)
//         |> Seq.toList

//     let d = distMap s
//     let maxEven = setCountsFiltered (fun i -> i % 2 = 0) s
//     let maxOdd = setCountsFiltered (fun i -> i % 2 = 1) s
//     let maxEvenLen = (Seq.length maxEven + 1) * 2
//     let maxOddLen = (Seq.length maxOdd + 1) * 2 + 1
//     let maxEvenSize = maxEven |> Seq.last
//     let maxOddSize = maxOdd |> Seq.last
//     printfn $"{label}"
//     //printfn $"{List.rev s}"
//     //printfn $"{d}"
//     let startDist = d |> Map.find start
//     printfn $"{maxEvenLen} {maxOddLen}"
//     printfn $"{maxEvenSize} {maxOddSize}"
//     printfn $"{startDist}"

//     {| FillDist = (fun x -> if x % 2 = 0 then maxEvenLen else maxOddLen)
//        FillSize = (fun x -> if x % 2 = 0 then maxEvenSize else maxOddSize)
//        CenterDist = startDist |}

// let floodFill maxCost dirsAndCost =
//     let rec loop seen todo =
//         match todo with
//         | [] -> seen
//         | (p, cost) :: acc ->
//             let seen = Set.add p seen

//             let todo =
//                 dirsAndCost
//                 |> Seq.map (fun (d, c) -> posPlus p d, cost + c)
//                 |> Seq.filter (fun (p, c) -> c <= maxCost && not (Set.contains p seen))
//                 |> Seq.toList

//             loop seen (todo @ acc)

//     loop Set.empty [ (0L, 0L), 0 ]

// let firstAreaInfo = seqIntoMax spaces |> maxInfo "1x1" spaces start
// //maxInfo "3x3" spaces3x3
// let seq3x3 = seqIntoMax spaces3x3
// printfn $"{seq3x3 |> Seq.length}"

// let areasInfo =
//     areas3x3
//     |> Map.map (fun d s -> seq3x3 |> maxInfo (sprintf "area %A" d) s (posPlus start (posMult inputDim d)))

// let dirsAndCost = areasInfo |> Map.map (fun _ i -> i.CenterDist) |> Map.toList

// let filledBySteps x =
//     floodFill x dirsAndCost |> Set.count |> (*) (firstAreaInfo.FillSize x)
//[1..50] |> List.map (countN spaces) |> List.iter (printfn "%A")

let part2 () = countN spaces 500
printfn $"{part2 ()}"
