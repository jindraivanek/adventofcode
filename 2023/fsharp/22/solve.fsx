#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

let memoize f =
    let cache = System.Collections.Generic.Dictionary<_, _>()

    fun x ->
        match cache.TryGetValue x with
        | true, v -> 
            //printfn "cache hit %A" (x,v)
            v
        | _ ->
            let v = f x
            cache.Add(x, v)
            v

let memoize2 f = memoize (fun (x, y) -> f x y) |> fun g -> fun x y -> g (x, y)

type Cube = int * int * int
type Brick = Set<Cube>

let posPlus (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)

let fallDir = (0, 0, -1)

let mkBrick (x1, y1, z1) (x2, y2, z2) =
    if x1 <> x2 then [min x1 x2 .. max x1 x2] |> List.map (fun x -> (x, y1, z1))
    elif y1 <> y2 then [min y1 y2 .. max y1 y2] |> List.map (fun y -> (x1, y, z1))
    else [min z1 z2 .. max z1 z2] |> List.map (fun z -> (x1, y1, z))
    |> set
let onGround b = Set.exists (fun (_,_,z) -> z <= 0) b

//let withoutBrick = memoize2 (fun bs b -> bs |> Seq.filter ((<>) b) |> Set.unionMany)

let fall otherCubes b =
    let rec go changed b =
        if onGround b then (if changed then Some b else None)
        else 
            let b2 = Set.map (posPlus fallDir) b
            if Set.exists (fun p -> Set.contains p otherCubes) b2 then (if changed then Some b else None)
            else go true b2
    go false b

let fallAll bricks =
    let cubes = bricks |> Set.unionMany
    let rec go changed cubes bs =
        let c2, bs2 =
            bs |> Seq.fold (fun (cubes, acc) b ->
                let c1 = cubes - b
                fall c1 b |> Option.map (fun b2 -> c1 + b2, b2::acc) |> Option.defaultValue (cubes, b::acc)) (cubes, [])
        if c2 = cubes then changed, set bs
        else go true c2 bs2
    go false cubes (Set.toList bricks)

let bricks = 
    lines
    |> Seq.map (fun l -> l.Split('~') |> Seq.map (fun s -> s.Split(',') |> Seq.map int |> Seq.toList) |> Seq.toList)
    |> Seq.map (fun [[x1;y1;z1];[x2;y2;z2]] -> mkBrick (x1,y1,z1) (x2,y2,z2))
    |> Seq.toList
    |> set

let falled = fallAll bricks |> snd

let canBeRemoved bricks b =
    let bs2 = bricks |> Set.remove b 
    fallAll bs2 |> fst |> not

printfn "%A" falled

let part1 = falled |> Set.filter (canBeRemoved falled) |> Set.count
let part2 = 0

printfn $"{part1}"
printfn $"{part2}"
