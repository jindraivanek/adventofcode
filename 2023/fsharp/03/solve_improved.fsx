#time

let lines = System.IO.File.ReadAllLines("input")
let posPlus (x1, y1) (x2, y2) = x1+x2, y1+y2
let neighbors p = 
    [ for x in -1..1 do for y in -1..1 do yield x, y] 
    |> List.map (posPlus p)
let withNeighbors ps = ps |> Seq.collect neighbors |> set

let indexed = lines |> Seq.mapi (fun i s -> s |> Seq.mapi (fun j c -> (i, j), c)) |> Seq.collect id
let symbols = indexed |> Seq.filter (fun (_, c) -> not (System.Char.IsAsciiDigit c || c = '.')) |> Seq.map fst
let symbolsWithNeighbors = symbols |> withNeighbors |> set
let gears = indexed |> Seq.filter (fun (_, c) -> c = '*') |> Seq.map fst
let gearsWithNeighbors = gears |> withNeighbors |> set
let numbers symbolsWithNeighbors =
    (([], [], false), indexed) ||> Seq.fold (fun (acc, w, nearSymbol) ((((i, _) as p), c) as x) ->
        if System.Char.IsDigit c && (w = [] || Some i = (w |> List.tryHead |> Option.map (fst >> fst))) then
            let nearSymbol = nearSymbol || (symbolsWithNeighbors |> Set.contains p)
            acc, x :: w, nearSymbol
        elif nearSymbol then
            let num = w |> List.map (snd >> string) |> List.rev |> String.concat "" |> int
            let numIndexes = w |> List.map fst |> List.rev
            (num, numIndexes) :: acc, [], false
        else acc, [], false)
    |> fun (acc, _, _) -> acc

let gearPairs =
    let nums = numbers gearsWithNeighbors
    gears |> Seq.map (fun g ->
        let gNeighbors = neighbors g |> set
        let nearNums = nums |> List.filter (fun (_, ps) -> Set.intersect (set ps) gNeighbors |> Set.isEmpty |> not)
        if List.length nearNums = 2 then nearNums |> List.map fst |> List.reduce (*) else 0
    )

let part1 = numbers symbolsWithNeighbors |> Seq.sumBy fst
let part2 = gearPairs |> Seq.sum

printfn $"{part1}"
printfn $"{part2}"
