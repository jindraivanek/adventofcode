module day08

open Common

let init (lines: string[]) =
    let m =
        lines
        |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> c, (i, j)))
        |> Seq.collect id

    let antennas =
        m
        |> Seq.filter (fun (c, _) -> c <> '.')
        |> Seq.groupBy fst
        |> Seq.map (fun (c, g) -> c, g |> Seq.map snd |> Seq.toList)
        |> Seq.toList

    {| Antennas = antennas
       MapSize = lines.[0].Length, lines.Length |}

let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let posMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let posMulC (x, y) c = (x * c, y * c)

let isInMap' (x, y) size =
    x >= 0 && x < fst size && y >= 0 && y < snd size

let allPairs xs =
    seq {
        for x1 in xs do
            for x2 in xs do
                if x1 <> x2 then
                    yield x1, x2
    }

let solve doRepeats lines =
    let s = init lines
    let repeats = if doRepeats then [ 0 .. lines.[0].Length ] else [ 1 ]

    let r =
        s.Antennas
        |> Seq.map snd
        |> Seq.collect (fun a ->
            let ps = a |> allPairs

            let antinodes =
                ps
                |> Seq.collect (fun (a, b) ->
                    let v = posMinus a b
                    repeats |> Seq.map (fun i -> posPlus a (posMulC v i)))
                |> Seq.filter (fun p -> isInMap' p s.MapSize)

            antinodes)
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toList

    r

let sol =
    { Day = 8
      Part1 = solution (solve false) (Seq.length >> string)
      Part2 = solution (solve true) (Seq.length >> string) // wrong: 931 too low
    }
