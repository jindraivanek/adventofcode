#time
let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/01.txt")

let numbers lines =
    lines
    |> Seq.map (fun s ->
        let first = s |> Seq.find System.Char.IsDigit
        let last = s |> Seq.findBack System.Char.IsDigit
        sprintf "%c%c" first last |> int)

let numberOrWords lines =
    let words =
        ([ 1..9 ] |> List.map (fun x -> string x, x))
        @ [ "one", 1
            "two", 2
            "three", 3
            "four", 4
            "five", 5
            "six", 6
            "seven", 7
            "eight", 8
            "nine", 9 ]

    lines
    |> Seq.map (fun (s: string) ->
        let first =
            words
            |> Seq.map (fun (w, n) -> s.IndexOf(w), n)
            |> Seq.filter (fun (i, _) -> i >= 0)
            |> Seq.minBy fst
            |> snd

        let last =
            words
            |> Seq.map (fun (w, n) -> s.LastIndexOf(w), n)
            |> Seq.filter (fun (i, _) -> i >= 0)
            |> Seq.maxBy fst
            |> snd

        sprintf "%s%s" (string first) (string last) |> int)

let part1 = numbers lines |> Seq.sum
let part2 = numberOrWords lines |> Seq.sum

printfn $"{part1}"
printfn $"{part2}"
