module day07

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2023/07.txt")

let parse isPart2 =
    lines
    |> Array.map (fun line ->
        let [| hand; score |] = line.Split(" ")
        let score = int64 score

        let hand =
            hand
            |> Seq.map (function
                | c when System.Char.IsDigit c -> int64 c - int64 '0'
                | 'T' -> 10L
                | 'J' -> if isPart2 then 1L else 11L
                | 'Q' -> 12L
                | 'K' -> 13L
                | 'A' -> 14L)
            |> Seq.toList

        hand, score)

type Kind =
    | HighCard
    | Pair
    | TwoPairs
    | Three
    | FullHouse
    | Four
    | Five

let kind hand =
    let counts =
        hand
        |> Seq.filter (fun x -> x > 1L)
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.filter (fun x -> x > 1)
        |> Seq.sortDescending
        |> Seq.toList

    let jokers = hand |> Seq.filter ((=) 1L) |> Seq.length

    let counts =
        match counts with
        | x :: xs -> x + jokers :: xs
        | [] when jokers = 5 -> [ 5 ]
        | [] -> [ jokers + 1 ]

    match counts with
    | [ 5 ] -> Five
    | [ 4 ] -> Four
    | [ 3; 2 ] -> FullHouse
    | [ 3 ] -> Three
    | [ 2; 2 ] -> TwoPairs
    | [ 2 ] -> Pair
    | _ -> HighCard


let ordered isPart2 =
    parse isPart2 |> Array.sortBy (fun (hand, _) -> kind hand, hand)
//ordered true |> Seq.map (fun (hand, _) -> kind hand, hand) |> Seq.iter (printfn "%A")

let part1 =
    ordered false
    |> Array.mapi (fun i (_, score) -> int64 (i + 1) * score)
    |> Array.sum

let part2 =
    ordered true
    |> Array.mapi (fun i (_, score) -> int64 (i + 1) * score)
    |> Array.sum

printfn $"{part1}"
printfn $"{part2}"
