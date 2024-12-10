module day10
open Common

let dirs = [ (0, -1); (-1, 0); (0, 1); (1, 0) ]
let init (lines: string[]) =
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> (i, j), int (string c))) |> Seq.collect id |> Map.ofSeq
    let starts = m |> Map.filter (fun _ v -> v = 0) |> Map.toSeq |> Seq.map fst
    let edges = 
    m

let part1 lines = 
   parseInput lines

let part2 lines = 
    parseInput lines

let sol = {
    Day = 10
    Part1 = solution part1 (Seq.length >> string)
    Part2 = solution part2 (Seq.length >> string)
}
