module day22
open Common

let parseInput (lines: string[]) =
    lines |> Seq.map int64 |> Seq.toList

let mix a b = a ^^^ b

let prune (x: int64) = x % 16777216L

let genSecretPart op (x: int64) =
    mix (op x) x |> prune

let genSecret x = 
    x |> genSecretPart ((*) 64L)
    |> genSecretPart (fun x -> x / 32L)
    |> genSecretPart ((*) 2048L)

let rec secretSeq x = seq {
    let y = genSecret x
    y
    yield! secretSeq y 
}

let part1 lines = 
   parseInput lines |> Seq.map (secretSeq >> Seq.nth 1999)

let part2 lines = 
    parseInput lines

let sol = {
    Day = 22
    Part1 = solution part1 (Seq.sum >> string)
    Part2 = solution part2 (Seq.length >> string)
}
