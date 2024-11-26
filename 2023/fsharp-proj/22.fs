module day22

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2023/22.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let posPlus (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)

let fallDir = (0, 0, -1)

let mkBrick (x1, y1, z1) (x2, y2, z2) =
    if x1 <> x2 then
        [ min x1 x2 .. max x1 x2 ] |> List.map (fun x -> (x, y1, z1))
    elif y1 <> y2 then
        [ min y1 y2 .. max y1 y2 ] |> List.map (fun y -> (x1, y, z1))
    else
        [ min z1 z2 .. max z1 z2 ] |> List.map (fun z -> (x1, y1, z))
    |> set

let onGround b = Set.exists (fun (_, _, z) -> z <= 0) b

let fall otherCubes b =
    let rec go changed b =
        if onGround b then
            (if changed then Some b else None)
        else
            let b2 = Set.map (posPlus fallDir) b

            if Set.exists (fun p -> Set.contains p otherCubes) b2 then
                (if changed then Some b else None)
            else
                go true b2

    go false b

let fallAll bricks =
    let cubes = bricks |> Set.unionMany
    let bricks = bricks |> Seq.indexed |> Seq.toList

    let rec go changed cubes bs =
        let c2, bs2 =
            bs
            |> Seq.fold
                (fun (cubes, acc) (label, b) ->
                    let c1 = cubes - b

                    fall c1 b
                    |> Option.map (fun b2 -> c1 + b2, (label, b2) :: acc)
                    |> Option.defaultValue (cubes, (label, b) :: acc))
                (cubes, [])

        let changed = (set bs2 - set bs) |> Set.map fst |> Set.union changed
        if c2 = cubes then changed, set bs else go changed c2 bs2

    go Set.empty cubes bricks
    |> fun (changed, bs) -> Set.count changed, bs |> Set.map snd

let bricks =
    lines
    |> Seq.map (fun l ->
        l.Split('~')
        |> Seq.map (fun s -> s.Split(',') |> Seq.map int |> Seq.toList)
        |> Seq.toList)
    |> Seq.map (fun [ [ x1; y1; z1 ]; [ x2; y2; z2 ] ] -> mkBrick (x1, y1, z1) (x2, y2, z2))
    |> Seq.toList
    |> set

let falled = fallAll bricks |> snd

let canBeRemoved bricks b =
    let bs2 = bricks |> Set.remove b
    fallAll bs2 |> fst

let part1 () =
    falled |> Set.filter (fun b -> canBeRemoved falled b = 0) |> Set.count

let part2 () =
    falled |> Seq.sumBy (fun b -> canBeRemoved falled b)

printfn $"{part1 ()}"
printfn $"{part2 ()}"
