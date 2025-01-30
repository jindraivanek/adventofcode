module day22

open Common

let parseInput (lines: string[]) = lines |> Seq.map int64 |> Seq.toList

let mix a b = a ^^^ b

let prune (x: int64) = x % 16777216L

let genSecretPart op (x: int64) = mix (op x) x |> prune

let genSecret x =
    x
    |> genSecretPart ((*) 64L)
    |> genSecretPart (fun x -> x / 32L)
    |> genSecretPart ((*) 2048L)

let rec secretSeq x =
    seq {
        yield x
        yield! secretSeq (genSecret x)
    }

let part1 lines =
    parseInput lines |> Seq.map (secretSeq >> Seq.nth 2000)

let secretSeq2 x =
    secretSeq x |> Seq.map (fun x -> string x |> Seq.last |> string |> int)

let secretComb init =
    secretSeq2 init
    |> Seq.take 2001
    |> Seq.windowed 5
    |> Seq.map (fun x ->
        let d =
            (x[1] - x[0]) * 1000000
            + (x[2] - x[1]) * 10000
            + (x[3] - x[2]) * 100
            + (x[4] - x[3])

        d, (init, x[4]))

let part2 lines =
    parseInput lines
    |> Seq.collect secretComb
    |> Seq.groupBy fst
    |> Seq.sortBy fst
    |> Seq.map (fun (k, g) ->
        let firstPerBuyer = g |> Seq.map snd |> Seq.groupBy fst |> Seq.map (snd >> Seq.head)
        let v = firstPerBuyer |> Seq.map snd |> Seq.sum
        k, v)
    |> Seq.maxBy snd

let sol =
    { Day = 22
      Part1 = solution part1 (Seq.sum >> string)
      Part2 = solution part2 (snd >> string) }
