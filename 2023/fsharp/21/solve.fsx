#r "nuget: Rationals"
#time

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/21.txt")
//let lines = System.IO.File.ReadAllLines("sample")
//let lines = System.IO.File.ReadAllLines("sample2")

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
            
let printGrid g start s =
    let minX = s |> Seq.map fst |> Seq.min
    let maxX = s |> Seq.map fst |> Seq.max
    let minY = s |> Seq.map snd |> Seq.min
    let maxY = s |> Seq.map snd |> Seq.max
    printfn "%A" ((minX, minY), (maxX, maxY))

    for y in minY..maxY do
        for x in minX..maxX do
            if (x,y) = start then printf "S"
            elif Set.contains (x, y) s then printf "*" 
            elif not (Set.contains (x, y) g) then printf "#"
            else printf "."

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

let memSetVariantSeqFunc f =
    memoize
    <| fun sv ->
        let s = fromSetVariant sv
        f s |> Seq.map toSetVariant |> set

let inline posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let inline posMult (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
let dirs = [ (0L, -1L); (-1L, 0L); (0L, 1L); (1L, 0L) ]

let printGrids dim g start ss =
    ss |> Map.toSeq |> Seq.collect (fun ((dx, dy), s) -> s |> Seq.map (fun x -> posMult dim (int64 dx, int64 dy) |> posPlus x)) |> set |> printGrid g start

let indexed =
    lines
    |> Array.mapi (fun y line ->
        line
        |> Seq.mapi (fun x c -> (int64 x, int64 y), c)
        )
    |> Seq.concat
    |> Seq.toList

let dimX = int64 lines[0].Length
let dimY = int64 lines.Length
let inputDim = dimX, dimY
//printfn $"{inputDim}"
let start = indexed |> Seq.find (fun (_, c) -> c = 'S') |> fst
let spaces = indexed |> Seq.filter (fun (_, c) -> c <> '#') |> Seq.map fst |> set
let isOutside (x, y) =
    x < 0L || y < 0L || x >= dimX || y >= dimY

let mirror (x, y) =
    if x < 0L then (-1, 0), (dimX + x, y)
    elif y < 0L then (0, -1), (x, dimY + y)
    elif x >= dimX then (1, 0), (x - dimX, y)
    elif y >= dimY then (0, 1), (x, y - dimY)
    else (0, 0), (x, y)

let step spaces p =
    dirs
    |> Seq.map (posPlus p)
    |> Seq.filter (fun p -> Set.contains p spaces || isOutside p)
    |> Seq.map mirror

let allSteps =
    memSetVariantSeqFunc
    <| fun (_, s) ->
        s
        |> Seq.collect (step spaces)
        |> Seq.groupBy fst
        |> Seq.map (fun (p, xs) -> p, xs |> Seq.map snd |> set)

let apply =
    memoize
    <| fun plots ->
        plots
        |> Map.toSeq
        |> Seq.collect (fun (area, s) ->
            allSteps s
            |> Seq.map fromSetVariant
            |> Seq.map (fun (areaDir, x) -> posPlus area areaDir, x))
        |> Seq.groupBy fst
        |> Seq.map (fun (p, xs) -> p, toSetVariant (p, xs |> Seq.map snd |> Set.unionMany))
        |> Map.ofSeq

let allStepsSeq _ plots =
    Seq.unfold (fun s -> Some(s, apply s)) plots

let allStepsN g s n =
    allStepsSeq g s
    |> Seq.indexed
    |> Seq.map (fun (i, x) ->
        x)
    |> Seq.nth n

let startPlots = Map.ofSeq [ (0, 0), toSetVariant ((0, 0), set [ start ]) ]

let startSeq spaces =
    allStepsSeq spaces startPlots |> Seq.cache

let countN spaces n =
    let m = allStepsN spaces startPlots n 
    //printGrids inputDim spaces start (Map.map (fun _ s -> fromSetVariant s |> snd) m)
    m |> Map.values |> Seq.sumBy (fromSetVariant >> snd >> Set.count)

let part1 () = countN spaces 64
printfn $"{part1()}"

open Rationals

let lagrangePolynomInterpolation (xs: seq<int>) (ys: seq<int>) x =
    let xs = Array.ofSeq xs |> Array.map Rational
    let ys = Array.ofSeq ys |> Array.map Rational
    let x = Rational x
    let n = xs.Length
    let p =
        [0 .. n-1] |> List.map (fun i ->
            let mutable term = Rational 1
            for j in 0..n-1 do
                if i <> j then
                    term <- term * (x - xs.[j]) / (xs.[i] - xs.[j])
            term)
    [0 .. n-1] |> List.map (fun i -> ys.[i] * p.[i]) |> List.reduce (+) |> fun x -> x.CanonicalForm

let stepsData n =
    let d = 
        startSeq spaces |> Seq.map (fun m -> m |> Map.values |> Seq.sumBy (fromSetVariant >> snd >> Set.count)) |> Seq.indexed 
        |> Seq.take n |> Seq.toArray
    //d |> Seq.iter (printfn "%A")
    d
let part2 () = 
    let size = fst inputDim |> int
    let n = 2*size + size/2
    let xs, ys = stepsData n |> Array.filter (fun (x,_) -> (x - 65) % 131 = 0) |> Array.unzip
    lagrangePolynomInterpolation xs ys 26501365

printfn $"{part2 ()}"

