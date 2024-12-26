module day25

open Common

let parseInput (lines: string[]) =
    let schematics = lines |> splitBy ((=) "") |> Seq.toList
    let lockSch, keySch = schematics |> List.partition (fun s -> s[0] = "#####")

    let toWidths f s =
        s
        |> List.map (Seq.toList)
        |> List.transpose
        |> List.map (f >> List.tail >> List.takeWhile ((=) '#') >> List.length)

    let locks = lockSch |> List.map (toWidths id)
    let keys = keySch |> List.map (toWidths List.rev)
    locks, keys

let part1 lines =
    let locks, keys = parseInput lines

    List.allPairs locks keys
    |> List.filter (fun (l, k) ->
        let xs = List.zip l k
        xs |> List.forall (fun (l, k) -> l + k <= 5))

let part2 lines = parseInput lines

let sol =
    { Day = 25
      Part1 = solution part1 (Seq.length >> string)
      Part2 = solution part2 (string) }
