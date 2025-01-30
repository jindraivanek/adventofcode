module day01

open Common

let parseInput (lines: string[]) =
    lines
    |> Seq.map (fun s ->
        s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> fun xs -> xs[0], xs[1])
    |> Seq.toList

let part1 lines =
    let l1, l2 = parseInput lines |> List.unzip
    ((List.sort l1), (List.sort l2)) ||> List.map2 (fun x y -> abs (x - y))

let part2 lines =
    let l1, l2 = parseInput lines |> List.unzip
    let counts = List.countBy id l2 |> Map.ofList
    l1 |> List.map (fun x -> Map.tryFind x counts |> Option.defaultValue 0 |> (*) x)

let sol =
    { Day = 1
      Part1 = solution part1 (Seq.sum >> string)
      Part2 = solution part2 (Seq.sum >> string) }
