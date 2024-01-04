(*
--- Day 2: Cube Conundrum ---
You're launched high into the atmosphere! The apex of your trajectory just barely reaches the surface of a large island floating in the sky. You gently land in a fluffy pile of leaves. It's quite cold, but you don't see much snow. An Elf runs over to greet you.

The Elf explains that you've arrived at Snow Island and apologizes for the lack of snow. He'll be happy to explain the situation, but it's a bit of a walk, so you have some time. They don't get many visitors up here; would you like to play a game in the meantime?
...

SAMPLE:
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

...

The Elf would first like to know which games would have been possible 
if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?
*)

#time
open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let lines = System.IO.File.ReadAllLines("input")

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
