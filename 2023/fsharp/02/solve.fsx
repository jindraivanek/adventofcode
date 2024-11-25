#time
open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let lines = System.IO.File.ReadAllLines("__SOURCE_DIRECTORY__/../../../../input/2023/02.txt")

type Cube =
    | Red
    | Green
    | Blue

let parse line =
    match line with
    | Match "Game ([0-9]+): (.*)" [ gameNumber; game ] ->
        let cubes =
            game.Split([| ','; ';' |])
            |> Array.map (function
                | Match "([0-9]+) red" [ x ] -> Red, (int x)
                | Match "([0-9]+) green" [ x ] -> Green, (int x)
                | Match "([0-9]+) blue" [ x ] -> Blue, (int x))
            |> Seq.toList

        int gameNumber, cubes

let filterGames games =
    games
    |> Seq.filter (fun (_, cubes) ->
        cubes
        |> Seq.exists (function
            | Red, x -> x > 12
            | Green, x -> x > 13
            | Blue, x -> x > 14)
        |> not)
    |> Seq.map fst

let gamePower (_, cubes) =
    cubes
    |> List.groupBy fst
    |> List.map (fun (_, cubes) -> cubes |> List.map snd |> List.max)
    |> List.reduce (*)

let part1 = lines |> Seq.map parse |> filterGames |> Seq.sum
let part2 = lines |> Seq.map parse |> Seq.map gamePower |> Seq.sum

printfn $"{part1}"
printfn $"{part2}"
