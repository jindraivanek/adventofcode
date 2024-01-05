#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

let galaxies =
    lines
    |> Seq.mapi (fun i s -> s |> Seq.mapi (fun j c -> (j, i), c))
    |> Seq.collect id
    |> Seq.filter (fun (_, c) -> c = '#')
    |> Seq.map fst
    |> Seq.toList

let emptyIndex lines =
    lines
    |> Seq.mapi (fun i s -> i, s |> Seq.forall ((=) '.'))
    |> Seq.filter snd
    |> Seq.map fst
    |> Set.ofSeq

let emptyY = emptyIndex lines
let emptyX = emptyIndex (Seq.transpose lines)

let galaxyPairs =
    galaxies
    |> Seq.mapi (fun i g1 -> galaxies |> Seq.skip (i + 1) |> Seq.map (fun g2 -> g1, g2))
    |> Seq.collect id
    |> Seq.toList

let dist eSize (x1, y1) (x2, y2) =
    let dx = abs (x1 - x2) |> int64
    let dy = abs (y1 - y2) |> int64

    let ex =
        [ min x1 x2 .. max x1 x2 ]
        |> Seq.filter (fun x -> Set.contains x emptyX)
        |> Seq.length
        |> int64

    let ey =
        [ min y1 y2 .. max y1 y2 ]
        |> Seq.filter (fun y -> Set.contains y emptyY)
        |> Seq.length
        |> int64

    let d = dx + dy + ex * (eSize - 1L) + ey * (eSize - 1L)
    d

let part1 = galaxyPairs |> Seq.map (fun (g1, g2) -> dist 2 g1 g2) |> Seq.sum
let part2 = galaxyPairs |> Seq.map (fun (g1, g2) -> dist 1000000 g1 g2) |> Seq.sum

printfn $"{part1}"
printfn $"{part2}"
