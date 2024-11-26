#time
open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/04.txt")

let parse lines =
    lines
    |> Seq.map (fun line ->
        match line with
        | Match "Card +([0-9]+): +(.*) +\| +(.*)" [ _; winning; numbers ] ->
            let winning =
                winning.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int
                |> Set.ofArray

            let numbers =
                numbers.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int

            let winCount = numbers |> Seq.filter (fun x -> Set.contains x winning) |> Seq.length
            winCount
        | _ -> failwith "invalid input")

let score x =
    if x = 0 then 0 else 2. ** float (x - 1) |> int

let cardCopies xs =
    let n = Seq.length xs
    let xs = xs |> Seq.indexed
    let m = xs |> Seq.map (fun (i, _) -> i, 1) |> Map.ofSeq

    let counts =
        (m, xs)
        ||> Seq.fold (fun m (i, x) ->
            (m, [ i + 1 .. i + x ] |> List.filter (fun j -> j < n))
            ||> Seq.fold (fun m j -> Map.add j (m[j] + m[i]) m))

    counts |> Map.toSeq |> Seq.sumBy snd

let part1 = lines |> parse |> Seq.map score |> Seq.sum
let part2 = lines |> parse |> cardCopies

printfn $"{part1}"
printfn $"{part2}"
