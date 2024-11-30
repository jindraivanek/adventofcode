module day01
open Common

let part1 lines = 
    lines

let part2 lines = 
    lines

let sol = {
    Day = 1
    Part1 = {
        Init = part1
        Step = fun _ -> None
        Result = Seq.length >> string
    }
    Part2 = {
        Init = part2
        Step = fun _ -> None
        Result = Seq.length >> string
    }
}
