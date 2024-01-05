#time

let lines = System.IO.File.ReadAllLines("input")
let posXDiff (x1, y1) (x2, y2) = abs (x1 - x2)
let posYDiff (x1, y1) (x2, y2) = abs (y1 - y2)
let posDiff (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let isNear p1 p2 =
    posDiff p1 p2 <= 2 && posXDiff p1 p2 <= 1 && posYDiff p1 p2 <= 1

let indexed =
    lines
    |> Seq.mapi (fun i s -> s |> Seq.mapi (fun j c -> (i, j), c))
    |> Seq.collect id

let symbols =
    indexed
    |> Seq.filter (fun (_, c) -> not (System.Char.IsAsciiDigit c || c = '.'))
    |> Seq.map fst

let gears = indexed |> Seq.filter (fun (_, c) -> c = '*') |> Seq.map fst

let numbers symbols =
    (([], [], false), indexed)
    ||> Seq.fold (fun (acc, w, nearSymbol) ((((i, _) as p), c) as x) ->
        if
            System.Char.IsDigit c
            && (w = [] || Some i = (w |> List.tryHead |> Option.map (fst >> fst)))
        then
            let nearSymbol = nearSymbol || (symbols |> Seq.exists (fun p2 -> isNear p p2))
            acc, x :: w, nearSymbol
        elif nearSymbol then
            let num = w |> List.map (snd >> string) |> List.rev |> String.concat "" |> int
            let numIndexes = w |> List.map fst |> List.rev
            (num, numIndexes) :: acc, [], false
        else
            acc, [], false)
    |> fun (acc, _, _) -> acc

let gearPairs =
    let nums = numbers gears

    gears
    |> Seq.map (fun g ->
        let nearNums =
            nums |> List.filter (fun (_, ps) -> ps |> Seq.exists (fun p -> isNear p g))

        if List.length nearNums = 2 then
            nearNums |> List.map fst |> List.reduce (*)
        else
            0)

let part1 = numbers symbols |> Seq.sumBy fst
let part2 = gearPairs |> Seq.sum

printfn $"{part1}"
printfn $"{part2}"
