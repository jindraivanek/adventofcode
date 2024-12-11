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

let solve n (i, xs) = 
   xs |> Seq.collect applyRules |> Seq.toList |> fun x -> 
   if i = n then None else 
    printfn "%A" x
    Some (i+1, x)

let part2 lines = 
    parseInput lines

let partSol n = {
    Init = parseInput >> fun x -> (0, x)
    Step = solve n
    Result = (fun (_,s) -> Seq.length s |> string)
}

let sol = {
    Day = 11
    Part1 = partSol 25
    Part2 = partSol 75
}
