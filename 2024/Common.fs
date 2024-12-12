module Common

type Solution<'s> = {
    Init: string[] -> 's
    Step: 's -> 's option
    Result: 's -> string
}

type Day<'s1, 's2> = {
    Day: int
    Part1: Solution<'s1>
    Part2: Solution<'s2>
}

let solution init result = { Init = init; Step = (fun _ -> None); Result = result }

let readLines (day: int) extra= 
    let filename = $"%s{__SOURCE_DIRECTORY__}/../input/2024/%02i{day}{extra}.txt"
    if System.IO.File.Exists(filename) then
        Some (System.IO.File.ReadAllLines(filename))
    else
        None
        

let benchmark label f =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let x = f()
    sw.Stop()
    printfn $"%s{label}: %A{x} [%A{sw.Elapsed}]"

let run init step = init |> Seq.unfold (fun s -> step s |> Option.map (fun x -> x, x))

let runSolution (sol: Solution<'s>) (lines: string[]) =
    let init = sol.Init lines 
    run init sol.Step  |> Seq.tryLast |> Option.defaultValue init |> sol.Result

let private runDay' extraInputName (day: Day<'s1, 's2>) =
    match readLines day.Day extraInputName with
    | Some lines ->
        benchmark $"Day %i{day.Day} - Part 1" (fun () -> runSolution day.Part1 lines)
        benchmark $"Day %i{day.Day} - Part 2" (fun () -> runSolution day.Part2 lines)
    | None ->
        printfn $"Input file for day %i{day.Day} does not exist"
        
let runDaySample (day: Day<'s1, 's2>) = runDay' "_sample" day
let runDay (day: Day<'s1, 's2>) = runDay' "" day

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

module Grid =
    let dirs = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
    let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    let posMult (x1, y1) c = (x1 * c, y1 * c)
