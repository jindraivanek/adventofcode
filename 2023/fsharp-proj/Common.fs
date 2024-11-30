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

let readLines (day: int) = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2023/%02i{day}.txt")

let runSolution (sol: Solution<'s>) (lines: string[]) =
    let init = sol.Init lines 
    init |> Seq.unfold (fun s -> sol.Step s |> Option.map (fun x -> x, x)) |> Seq.tryLast |> Option.defaultValue init |> sol.Result

let runDay (day: Day<'s1, 's2>) =
    let lines = readLines day.Day
    printfn "Day %i - Part 1: %s" day.Day (runSolution day.Part1 lines)
    printfn "Day %i - Part 2: %s" day.Day (runSolution day.Part2 lines)