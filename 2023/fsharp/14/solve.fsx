#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

let dirs = [ (0, -1); (-1, 0); (0, 1); (1, 0) ]
let posPlus (x1, y1) (x2, y2) = (x1+x2, y1+y2)

let indexed = lines |> Seq.mapi (fun i s -> s |> Seq.mapi (fun j c -> (j, i), c)) |> Seq.collect id
let space = indexed |> Seq.filter (snd >> (<>) '#') |> Seq.map fst |> set
let rocks = indexed |> Seq.filter (snd >> (=) 'O') |> Seq.map fst |> set
let cubes = indexed |> Seq.filter (snd >> (=) '#') |> Seq.map fst |> set

let rec slideRock moveDir p rocks =
    let p2 = posPlus p moveDir
    if Set.contains p2 space && not (Set.contains p2 cubes) && not (Set.contains p2 rocks) then
        let rocks2 = rocks |> Set.remove p |> Set.add p2
        slideRock moveDir p2 rocks2
    else rocks

let slideAllRocks moveDir rocks =
    let sortFun (x, y) = match moveDir with | (0, a) -> x, y * a * -1 | (a, 0) -> y, x * a * -1 | _ -> failwith "bad move"
    let ps = rocks |> Set.toList |> List.sortBy sortFun
    (rocks, ps) ||> List.fold (fun rocks p -> slideRock moveDir p rocks)

let n = lines.Length
let rocksLoad rocks = rocks |> Seq.sumBy (fun (_, y) -> n - y)

let rec slideAllRocksCycle i dirs rocks =
    printfn $"%A{i} {rocksLoad rocks}"
    //rocks |> Set.toList |> List.iter (printfn "%A")
    let rocks2 = dirs |> List.fold (fun rocks dir -> slideAllRocks dir rocks) rocks
    if rocks2 = rocks then rocks else slideAllRocksCycle (i+1) dirs rocks2

let part1() =
    let r = slideAllRocks dirs[0] rocks
    //printfn $"%A{r}"
    rocksLoad r
let part2() =
    let r = slideAllRocksCycle 0 dirs rocks
    //printfn $"%A{r}"
    rocksLoad r

printfn $"{part1()}" //107951
printfn $"{part2()}" //95736
