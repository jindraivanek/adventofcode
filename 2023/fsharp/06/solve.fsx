#time
open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/06.txt")

let races isPart2 =
    lines
    |> Seq.toList
    |> List.map (function
        | Match ".*: (.*)" [ nums ] ->
            if isPart2 then (nums.Replace(" ", "")) else nums
            |> fun nums ->
                nums.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int64
                |> Array.toList)
    |> List.transpose
    |> List.map (fun xs -> xs[0], xs[1])

let waysToWin t d =
    [ 1L .. t ] |> List.filter (fun i -> i * (t - i) > d) |> List.length

let part1 = races false |> List.map (fun (t, d) -> waysToWin t d) |> List.reduce (*)
let part2 = races true |> List.map (fun (t, d) -> waysToWin t d) |> List.reduce (*)

printfn $"{part1}"
printfn $"{part2}"
