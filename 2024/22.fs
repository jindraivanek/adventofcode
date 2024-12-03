module day22
open Common

let parseInput (lines: string[]) =
    lines

let part1 lines = 
   parseInput lines

let part2 lines = 
    parseInput lines

let sol = {
    Day = 22
    Part1 = solution part1 (Seq.length >> string)
    Part2 = solution part2 (Seq.length >> string)
}
