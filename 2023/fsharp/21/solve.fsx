#time

//let lines = System.IO.File.ReadAllLines("input")
let lines = System.IO.File.ReadAllLines("sample")

let printGrid s =
    let minX = s |> Seq.map fst |> Seq.min
    let maxX = s |> Seq.map fst |> Seq.max
    let minY = s |> Seq.map snd |> Seq.min
    let maxY = s |> Seq.map snd |> Seq.max
    printfn "%A" ((minX, minY), (maxX, maxY))
    for y in minY..maxY do
        for x in minX..maxX do
            if Set.contains (x, y) s then printf "#"
            else printf "."
        printfn ""
    printfn "----------------"

let inline mod_ a b = (a % b + b) % b

let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let posMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let posMult (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
let posModulo (x1, y1) (x2, y2) = (mod_ x1 x2), (mod_ y1 y2)
let dirNeg (x, y) = (-x, -y)
let dirs = [ (0L, -1L); (-1L, 0L); (0L, 1L); (1L, 0L) ]
let dirs3x3 = [ (-1L, -1L); (0L, -1L); (1L, -1L); (-1L, 0L); (1L, 0L); (-1L, 1L); (0L, 1L); (1L, 1L) ]

let indexed = lines |> Array.mapi (fun y line -> line |> Seq.mapi (fun x c -> (int64 x, int64 y), c) |> Seq.filter (fun (_, c) -> c <> '#')) |> Seq.concat |> Seq.toList
let inputDim = int64 lines[0].Length, int64 lines.Length
printfn $"{inputDim}"
let start = indexed |> Seq.find (fun (_, c) -> c = 'S') |> fst
let spaces = indexed |> Seq.map fst |> set
let spaces3x3 = dirs3x3 |> Seq.collect (fun d -> spaces |> Seq.map (posPlus (posMult inputDim d))) |> set
let getArea d s = 
    let (x0, y0) = posMult inputDim d
    let (x1, y1) = posMult inputDim d |> posPlus inputDim
    let x = s |> Set.filter (fun (x,y) -> x >= x0 && x <= x1 && y >= y0 && y <= y1)
    printGrid x
    x
let areas3x3 = dirs3x3 |> Seq.map (fun d -> d, getArea d spaces3x3) |> Map.ofSeq
let step spaces p = dirs |> Seq.map (posPlus p) |> Seq.filter (fun p -> Set.contains p spaces)
let allSteps g s = s |> Seq.collect (step g) |> set
let allStepsSeq g s = Seq.unfold (fun s -> Some(s, allSteps g s)) s
let allStepsN g s n = allStepsSeq g s |> Seq.indexed |> Seq.map (fun (i, x) -> (if i % 1 = 0 then printfn "%i" i); x) |> Seq.nth n
let startSeq spaces = allStepsSeq spaces (set [start]) |> Seq.cache
//allStepsSeq (set [start]) |> Seq.take 10 |> Seq.iter printGrid

let part1() = allStepsN spaces (set [start]) 64 |> Set.count //3847
//printfn $"{part1()}"

//let seqIntoMax s = startSeq s |> Seq.indexed |> Seq.pairwise |> Seq.takeWhile (fun ((_,s1), (_,s2)) -> Set.isEmpty s2 || Set.count s1 < Set.count s2) |> Seq.toList |> fun xs -> (0, Seq.head (startSeq s)) :: (List.map snd xs)
let seqIntoMax s = 
    let s = startSeq s |> Seq.cache
    let xs = s |> Seq.scan (fun (i, x, _) s -> printfn "%A" (i,x); let m = Set.count s in if m > x then (0, m, s) else (i+1, x, s)) (0, 0, Set.empty) |> Seq.takeWhile (fun (i,_,_) -> i <= 10) |> Seq.skip 1 |> Seq.map (fun (_,_,s) -> s)
    xs |> Seq.indexed |> Seq.toList
let setCountsFiltered f s = s |> List.filter (fun (i, x) -> (*printfn "i%i %i" i x;*) f i) |> List.map (snd >> Set.count)
let distMap s = s |> Seq.pairwise |> Seq.collect (fun ((_, x1), (i, x2)) -> x2-x1 |> Seq.map (fun x -> x, i)) |> Map.ofSeq
let maxInfo label area start s =
    let s = s |> Seq.map (fun (i, x) -> i, Set.filter (fun p -> Set.contains p area) x) |> Seq.toList 
    let d = distMap s
    let maxEven = setCountsFiltered (fun i -> i % 2 = 0) s
    let maxOdd = setCountsFiltered (fun i -> i % 2 = 1) s
    let maxEvenLen = (Seq.length maxEven + 1) * 2
    let maxOddLen = (Seq.length maxOdd + 1) * 2 + 1
    let maxEvenSize = maxEven |> Seq.last
    let maxOddSize = maxOdd |> Seq.last
    let startDist = d |> Map.find start
    printfn $"{label}"
    printfn $"{maxEvenLen} {maxOddLen}"
    printfn $"{maxEvenSize} {maxOddSize}"
    printfn $"{startDist}"
    {| EvenFillDist = maxEvenLen; OddFillDist = maxOddLen; EvenFillSize = maxEvenSize; OddFillSize = maxOddSize; CenterDist = startDist |}

let floodFill maxCost dirsAndCost start =
    let rec loop seen todo =
        match todo with
        | [] -> seen
        | (p, cost) :: todo ->
            let seen = Set.add p seen
            let todo = dirsAndCost |> Seq.map (fun (d, c) -> posPlus p d, cost + c) |> Seq.filter (fun (p, c) -> c <= maxCost && not (Set.contains p seen)) |> Seq.toList
            loop seen todo
    loop Set.empty [(start, 0)]

let firstAreaInfo = seqIntoMax spaces |> maxInfo "1x1" spaces start
//maxInfo "3x3" spaces3x3
let seq3x3 = seqIntoMax spaces3x3
printfn $"{seq3x3 |> Seq.length}"
let areasInfo = areas3x3 |> Map.map (fun d s -> seq3x3 |> maxInfo (sprintf "area %A" d) s (posPlus start (posMult inputDim d)))
let dirsAndCost = areasInfo |> Map.map (fun _ i -> i.CenterDist) |> Map.toList
let filledBySteps x = floodFill x dirsAndCost start |> Set.count
let part2() = filledBySteps 50

//printfn $"{part2()}"
