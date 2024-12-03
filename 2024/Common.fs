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

let readLines (day: int) = 
    let filename = $"%s{__SOURCE_DIRECTORY__}/../input/2024/%02i{day}.txt"
    if System.IO.File.Exists(filename) then
        Some (System.IO.File.ReadAllLines(filename))
    else
        None
        

let benchmark label f =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let x = f()
    sw.Stop()
    printfn $"%s{label}: %A{x} [%A{sw.Elapsed}]"

let runSolution (sol: Solution<'s>) (lines: string[]) =
    let init = sol.Init lines 
    init |> Seq.unfold (fun s -> sol.Step s |> Option.map (fun x -> x, x)) |> Seq.tryLast |> Option.defaultValue init |> sol.Result

let runDay (day: Day<'s1, 's2>) =
    match readLines day.Day with
    | Some lines ->
        benchmark $"Day %i{day.Day} - Part 1" (fun () -> runSolution day.Part1 lines)
        benchmark $"Day %i{day.Day} - Part 2" (fun () -> runSolution day.Part2 lines)
    | None ->
        printfn $"Input file for day %i{day.Day} does not exist"

open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None