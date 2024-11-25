#time

let lines = System.IO.File.ReadAllLines("__SOURCE_DIRECTORY__/../../../../input/2023/16.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let printGridSet (set: Set<(int * int)>) =
    let minX = set |> Set.map fst |> Seq.min
    let maxX = set |> Set.map fst |> Seq.max
    let minY = set |> Set.map snd |> Seq.min
    let maxY = set |> Set.map snd |> Seq.max

    for y in minY..maxY do
        for x in minX..maxX do
            if Set.contains (x, y) set then printf "#" else printf "."

        printfn ""

let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let L = (-1, 0)
let R = (1, 0)
let U = (0, -1)
let D = (0, 1)

let mirrorLeft =
    function
    | p when p = R -> U
    | p when p = U -> R
    | p when p = L -> D
    | p when p = D -> L
    | p -> failwith $"mirrorLeft: {p}"

let mirrorRight =
    function
    | p when p = R -> D
    | p when p = U -> L
    | p when p = L -> U
    | p when p = D -> R
    | p -> failwith $"mirrorRight: {p}"

let grid =
    lines
    |> Seq.mapi (fun y s -> s |> Seq.mapi (fun x c -> (x, y), c))
    |> Seq.collect id
    |> Map.ofSeq

let allIndexes = grid |> Map.keys |> set

let beamChangers =
    grid
    |> Map.filter (fun _ c -> c <> '.')
    |> Map.map (fun _ ->
        function
        | '/' -> fun d -> [ mirrorLeft d ]
        | '\\' -> fun d -> [ mirrorRight d ]
        | '|' -> fun d -> if d = U || d = D then [ d ] else [ U; D ]
        | '-' -> fun d -> if d = L || d = R then [ d ] else [ L; R ])

let rec moveBeam visitied (p, d) =
    if not (Set.contains p allIndexes) || Set.contains (p, d) visitied then
        visitied
    else
        //printfn $"%A{p} {d}"
        let v = Set.add (p, d) visitied

        let ds =
            match beamChangers |> Map.tryFind p with
            | None -> [ d ]
            | Some f -> f d

        ds |> List.fold (fun v d -> moveBeam v (posPlus p d, d)) v

let beamResult starts =
    starts |> List.map (fun x -> moveBeam Set.empty x |> Set.map fst)

//printGridSet beamResult

let allStarts =
    let maxX = allIndexes |> Seq.maxBy fst |> fst
    let maxY = allIndexes |> Seq.maxBy snd |> snd

    [ for x in 0..maxX do
          yield (x, 0), D
      for y in 0..maxY do
          yield (0, y), R
      for x in 0..maxX do
          yield (x, maxY), U
      for y in 0..maxY do
          yield (maxX, y), L ]

let part1 = beamResult [ (0, 0), R ] |> List.head |> Set.count
let part2 = beamResult allStarts |> List.map Set.count |> List.max

printfn $"{part1}"
printfn $"{part2}"
