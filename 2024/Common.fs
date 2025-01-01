module Common

type Solution<'s> = {
    Init: string[] -> 's
    Step: int ->'s -> 's option
    Result: 's -> string
}

type Day<'s1, 's2> = {
    Day: int
    Part1: Solution<'s1>
    Part2: Solution<'s2>
}

let solution init result = { Init = init; Step = (fun _ _ -> None); Result = result }

let readLines (day: int) extra= 
    let filename = $"%s{__SOURCE_DIRECTORY__}/../input/2024/%02i{day}{extra}.txt"
    if System.IO.File.Exists(filename) then
        Some (System.IO.File.ReadAllLines(filename))
    else
        None
        
let tee f x = f x; x

let benchmark label f =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let x = f()
    sw.Stop()
    printfn $"%s{label}: %A{x} [%A{sw.Elapsed}]"

let run init step = (0, init) |> Seq.unfold (fun (i, s) -> step i s |> Option.map (fun x -> x, (i+1, x))) |> Seq.append [init]

let runSolution (sol: Solution<'s>) (lines: string[]) =
    let init = sol.Init lines 
    run init sol.Step |> Seq.tryLast |> Option.defaultValue init |> sol.Result

let private runDay' extraInputName (day: Day<'s1, 's2>) =
    match readLines day.Day extraInputName with
    | Some lines ->
        benchmark $"Day %i{day.Day} - Part 1" (fun () -> runSolution day.Part1 lines)
        benchmark $"Day %i{day.Day} - Part 2" (fun () -> runSolution day.Part2 lines)
    | None ->
        printfn $"Input file for day %i{day.Day} does not exist"
        
let runDaySample (day: Day<'s1, 's2>) = runDay' "_sample" day
let runDay (day: Day<'s1, 's2>) = runDay' "" day

//-----

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let memoize f =
    let cache = System.Collections.Generic.Dictionary<_, _>()
    fun x ->
        match cache.TryGetValue(x) with
        | true, v -> v
        | false, _ ->
            let v = f x
            cache.Add(x, v)
            v

let memoizeRec f =
    let cache = System.Collections.Generic.Dictionary<_, _>()

    let rec f' x =
        match cache.TryGetValue x with
        | true, v -> v
        | _ ->
            let v = f f' x
            cache.Add(x, v)
            v

    f'

let memoizeBy g f =
    let cache = System.Collections.Generic.Dictionary<_, _>()
    fun x ->
        let k = g x
        match cache.TryGetValue(k) with
        | true, v -> v
        | false, _ ->
            let v = f x
            cache.Add(k, v)
            v

let splitBy f xs  =
    let rec go xs = seq {
        let l = Seq.length xs
        if l > 0 then
            let part = Seq.takeWhile (f >> not) xs |> Seq.toList
            yield part
            yield! go (Seq.skip (min l (part.Length + 1)) xs)
    }
    go xs

module Seq =
    let mapWithState f init xs = 
        let mutable state = init
        xs |> Seq.map (fun x -> let (s, y) = f state x in state <- s; y)
    let groupByAndMap f g xs =
        xs
        |> Seq.groupBy f
        |> Seq.map (fun (k, vs) -> k, g vs)

module Grid =
    let dirs = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
    let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    let posMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
    let posMult (x1, y1) c = (x1 * c, y1 * c)
    let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

module Graph =
    let floydWarshall (nodes: seq<'n>) edges =
        let nodes = nodes |> Seq.toArray 
        let n = Seq.length nodes
        let nodeIndex = nodes |> Seq.indexed |> Seq.map (fun (i, v) -> v, i) |> Map.ofSeq
        let index v = nodeIndex[v]
        let dist = Array2D.init n n (fun i j -> if i = j then 0 else 99999)
        for (u, v) in edges |> Seq.map (fun (u, v) -> index u, index v) do
            dist[u,v] <- 1
        for v in 0 .. n-1 do
            dist[v,v] <- 0
        for k in 0 .. n-1 do
            for i in 0 .. n-1 do
                for j in 0 .. n-1 do
                    if dist[i,j] > dist[i,k] + dist[k,j] 
                    then dist[i,j] <- dist[i,k] + dist[k,j]
        seq {
            for i in 0 .. n-1 do
                for j in 0 .. n-1 do
                    yield ((nodes[i], nodes[j]), dist[i,j])
        } |> Map.ofSeq

