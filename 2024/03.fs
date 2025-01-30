module day03

open Common

let parseInput (lines: string[]) = lines |> String.concat ""

let getMuls (s: string) =
    let mulRegex =
        System.Text.RegularExpressions.Regex "mul\(([0-9]{1,3}),([0-9]{1,3})\)"

    let matches = mulRegex.Matches s

    matches
    |> Seq.map (fun m -> List.tail [ for g in m.Groups -> g.Value ])
    |> Seq.map (fun s -> s |> List.map int)
    |> Seq.map (fun xs -> int64 xs[0] * int64 xs[1])
    |> Seq.toList

let getMuls2 (s: string) =
    let mulRegex =
        System.Text.RegularExpressions.Regex "mul\(([0-9]{1,3}),([0-9]{1,3})\)|do\(\)|don't\(\)"

    let matches = mulRegex.Matches s

    let instr =
        matches
        |> Seq.map (fun m -> m.Value, List.tail [ for g in m.Groups -> g.Value ])

    let mutable enabled = true

    instr
    |> Seq.filter (function
        | ("do()", _) ->
            enabled <- true
            false
        | ("don't()", _) ->
            enabled <- false
            false
        | _ -> enabled)
    |> Seq.map (fun (_, s) -> s |> List.map int)
    |> Seq.map (fun xs -> int64 xs[0] * int64 xs[1])
    |> Seq.toList

let part1 lines = parseInput lines |> getMuls

let part2 lines = parseInput lines |> getMuls2

let sol =
    { Day = 3
      Part1 = solution part1 (Seq.sum >> string)
      Part2 = solution part2 (Seq.sum >> string) }
