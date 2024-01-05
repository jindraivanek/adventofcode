(*

--- Day 14: Parabolic Reflector Dish ---
You reach the place where all of the mirrors were pointing: a massive parabolic reflector dish attached to the side of another large mountain.

...

O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
Start by tilting the lever so all of the rocks will slide north as far as they will go:

OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....

...

Tilt the platform so that the rounded rocks all roll north. Afterward, what is the total load on the north support beams?

--- Part Two ---
The parabolic reflector dish deforms, but not in a way that focuses the beam. To do that, you'll need to move the rocks to the edges of the platform. Fortunately, a button on the side of the control panel labeled "spin cycle" attempts to do just that!

...

Run the spin cycle for 1000000000 cycles. Afterward, what is the total load on the north support beams?

*)

#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

let dirs = [ (0, -1); (-1, 0); (0, 1); (1, 0) ]
let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let indexed =
    lines
    |> Seq.mapi (fun i s -> s |> Seq.mapi (fun j c -> (j, i), c))
    |> Seq.collect id

let space = indexed |> Seq.filter (snd >> (<>) '#') |> Seq.map fst |> set
let rocks = indexed |> Seq.filter (snd >> (=) 'O') |> Seq.map fst |> set
let cubes = indexed |> Seq.filter (snd >> (=) '#') |> Seq.map fst |> set

let rec slideRock moveDir p rocks =
    let p2 = posPlus p moveDir

    if
        Set.contains p2 space
        && not (Set.contains p2 cubes)
        && not (Set.contains p2 rocks)
    then
        let rocks2 = rocks |> Set.remove p |> Set.add p2
        slideRock moveDir p2 rocks2
    else
        rocks

let slideAllRocks moveDir rocks =
    let sortFun (x, y) =
        match moveDir with
        | (0, a) -> x, y * a * -1
        | (a, 0) -> y, x * a * -1
        | _ -> failwith "bad move"

    let ps = rocks |> Set.toList |> List.sortBy sortFun
    (rocks, ps) ||> List.fold (fun rocks p -> slideRock moveDir p rocks)

let n = lines.Length

let rocksLoad rocks =
    rocks |> Seq.sumBy (fun (_, y) -> n - y)

let part1 () =
    let r = slideAllRocks dirs[0] rocks
    //printfn $"%A{r}"
    rocksLoad r

let rec slideAllRocksCycle visited dirs rocks =
    let i = Map.count visited
    let visited2 = Map.add rocks i visited
    //printfn $"%A{i} {rocksLoad rocks}"
    //rocks |> Set.toList |> List.iter (printfn "%A")
    let rocks2 = dirs |> List.fold (fun rocks dir -> slideAllRocks dir rocks) rocks

    Map.tryFind rocks2 visited
    |> Option.map (fun j -> j, i - j + 1, visited2)
    |> Option.defaultWith (fun () -> slideAllRocksCycle visited2 dirs rocks2)

let part2 () =
    let cycleCount = 1000000000
    let offset, cycleLen, history = slideAllRocksCycle Map.empty dirs rocks

    let values =
        history |> Map.toList |> List.map (fun (r, i) -> i, rocksLoad r) |> Map.ofList

    let target = offset + (cycleCount - offset) % cycleLen
    values.[target]

printfn $"{part1 ()}" //107951
printfn $"{part2 ()}" //95736
