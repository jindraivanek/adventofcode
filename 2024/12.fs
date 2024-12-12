module day12
open Common
open Common.Grid

let parseInput (lines: string[]) =
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> (i, j), c))
    {| Map = m; Size = lines.[0].Length, lines.Length |}

let floodFill p m =
    let rec recF p m =

let part1 lines = 
   let s = parseInput lines
   s.Map

let part2 lines = 
    parseInput lines

let sol = {
    Day = 12
    Part1 = solution part1 (Seq.length >> string)
    Part2 = solution part2 (_.Map >> Seq.length >> string)
}
