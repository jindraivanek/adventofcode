#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

let printGrid s =
    let minX = s |> Seq.map fst |> Seq.min
    let maxX = s |> Seq.map fst |> Seq.max
    let minY = s |> Seq.map snd |> Seq.min
    let maxY = s |> Seq.map snd |> Seq.max
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
let dirs3x3 = [ (-1L, -1L); (0L, -1L); (1L, -1L); (-1L, 0L); (0L, 0L); (1L, 0L); (-1L, 1L); (0L, 1L); (1L, 1L) ]

let indexed = lines |> Array.mapi (fun y line -> line |> Seq.mapi (fun x c -> (int64 x, int64 y), c) |> Seq.filter (fun (_, c) -> c <> '#')) |> Seq.concat |> Seq.toList
let inputDim = int64 lines[0].Length, int64 lines.Length 
let start = indexed |> Seq.find (fun (_, c) -> c = 'S') |> fst
let spaces = indexed |> Seq.map fst |> set
let spaces3x3 = dirs3x3 |> Seq.collect (fun d -> spaces |> Seq.map (posPlus (posMult inputDim d))) |> set
let getArea d s = 
    let (x0, y0) = posMult inputDim d
    let (x1, y1) = posMult inputDim d |> posPlus (1L, 1L)
    s |> Set.filter (fun (x,y) -> x >= x0 && x <= x1 && y >= y0 && y <= y1)
let areas3x3 = dirs3x3 |> Seq.map (fun d -> d, getArea d spaces) |> Map.ofSeq
let step spaces p = dirs |> Seq.map (posPlus p) |> Seq.filter (fun p -> Set.contains p spaces)
let allSteps g s = s |> Seq.collect (step g) |> set
let allStepsSeq g s = Seq.unfold (fun s -> Some(s, allSteps g s)) s
let allStepsN g s n = allStepsSeq g s |> Seq.indexed |> Seq.map (fun (i, x) -> (if i % 1 = 0 then printfn "%i" i); x) |> Seq.nth n
let startSeq spaces = allStepsSeq spaces (set [start]) |> Seq.cache
//allStepsSeq (set [start]) |> Seq.take 10 |> Seq.iter printGrid

let part1() = allStepsN spaces (set [start]) 64 |> Set.count //3847
//printfn $"{part1()}"

let seqIntoMax f s = startSeq s |> Seq.map Set.count |> Seq.indexed |> Seq.filter (fun (i, x) -> (*printfn "i%i %i" i x;*) f i) |> Seq.map snd |> Seq.pairwise |> Seq.takeWhile (fun (s1, s2) -> s2 = 0 || s1 < s2) |> Seq.toList
let maxInfo label spaces =
    let maxEven = seqIntoMax (fun i -> i % 2 = 0) spaces
    let maxOdd = seqIntoMax (fun i -> i % 2 = 1) spaces
    let maxEvenLen = (Seq.length maxEven + 1) * 2
    let maxOddLen = (Seq.length maxOdd + 1) * 2 + 1
    let maxEvenSize = maxEven |> Seq.last |> snd
    let maxOddSize = maxOdd |> Seq.last |> snd
    printfn $"{label}"
    printfn $"{maxEvenLen} {maxOddLen}"
    printfn $"{maxEvenSize} {maxOddSize}"
maxInfo "1x1" spaces
//maxInfo "3x3" spaces3x3
areas3x3 |> Map.iter (fun d s -> maxInfo (sprintf "area %A" d) s)
let part2() = 0

//printfn $"{part2()}"
