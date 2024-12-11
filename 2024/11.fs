module day11
open Common

let parseInput (lines: string[]) =
    lines |> Seq.head |> fun s -> s.Split(' ') |> Seq.map int64 |> Seq.toList

let applyRules x =
    let s = string x
    match x with
    | 0L -> [1L]
    | _ when s.Length % 2 = 0 -> [int64 s[0..s.Length/2 - 1]; int64 s[s.Length/2..]]
    | _ -> [x * 2024L]

let stoneSizeMem = memoizeRec <| fun recF (n, x) ->
    if n = 0 then 1L
    else applyRules x |> Seq.sumBy (fun y -> recF ((n-1), y))

let solveMem n xs = xs |> Seq.map (fun x -> stoneSizeMem (n, x))

let sol = {
    Day = 11
    Part1 = solution parseInput (solveMem 25 >> Seq.sum >> string)
    Part2 = solution parseInput (solveMem 75 >> Seq.sum >> string)
}
