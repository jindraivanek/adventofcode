module day02
open Common

let parseInput (lines: string[]) =
    lines |> Seq.map (fun s -> s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Array.toList) |> Seq.toList

let diffs xs = xs |> List.pairwise |> List.map (fun (a, b) -> b - a)

let isSafe xs =
    let d = diffs xs
    List.forall (fun x -> x >= 1 && x <= 3) d || List.forall (fun x -> x <= -1 && x >= -3) d

let part1 lines = 
   parseInput lines |> List.filter (fun xs -> isSafe xs )

let part2 lines = 
    parseInput lines |> List.filter (fun xs -> 
        let variants = [0..List.length xs] |> List.map (fun i -> xs |> List.indexed |> List.filter (fun (j, _) -> i <> j) |> List.map snd)
        variants |> List.exists (fun xs -> isSafe xs))

let sol = {
    Day = 2
    Part1 = solution part1 (Seq.length >> string)
    Part2 = solution part2 (Seq.length >> string)
}
