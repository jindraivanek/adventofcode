#time

let lines = System.IO.File.ReadAllLines("__SOURCE_DIRECTORY__/../../../../input/2023/18.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let teePrint x =
    printfn "%A" x
    x

let teeAssert f x =
    if not (f x) then failwithf "Assertion failed: %A" x else x

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
let posMult (x, y) n = (x * n, y * n)
let posMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let posDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
let dirNeg (x, y) = (-x, -y)
let dirsMap = [ 'U', (0, -1); 'L', (-1, 0); 'D', (0, 1); 'R', (1, 0) ] |> Map.ofSeq
let dirs = dirsMap |> Map.toList |> List.map snd

let parsed =
    lines
    |> Seq.map (fun s -> let [| d; x; c |] = s.Split(' ') in Seq.head d, int x, c[2..6], c[7])

let instr isPart1 =
    parsed
    |> Seq.map (fun (d, x, hexLength, hexDir) ->
        let d, x =
            if isPart1 then
                d, x
            else
                let dirOrder = [ 'R'; 'D'; 'L'; 'U' ]
                let d = dirOrder.[int (string hexDir)]
                let x = System.Convert.ToInt32(hexLength, 16)
                d, x

        Map.find d dirsMap, x)

let grid isPart1 =
    instr isPart1
    |> Seq.map (fun (d, x) -> Seq.init x (fun _ -> d))
    |> Seq.concat
    |> Seq.fold (fun (s, p) d -> Set.add p s, posPlus p d) (Set.empty, (0, 0))
    |> fst

let intervals isPart1 =
    instr isPart1
    |> Seq.fold (fun (acc, p) (d, x) -> let p2 = posPlus p (posMult d x) in (p, p2) :: acc, p2) ([], (0, 0))
    |> fst
    |> List.map (fun (p1, p2) -> (min p1 p2, max p1 p2))
    |> List.sortBy (snd >> snd)

let gridMapFromIntervals intervals =
    let xs, ys =
        intervals
        |> List.collect (fun ((x1, y1), (x2, y2)) -> [ x1, y1; x2, y2 ])
        |> List.unzip

    let xs = xs |> List.sort |> List.distinct
    let ys = ys |> List.sort |> List.distinct

    xs
    |> List.mapi (fun i x -> ys |> List.mapi (fun j y -> (x, y), (i, j)))
    |> List.concat
    |> Map.ofSeq

let pointsToLine (x1, y1) (x2, y2) =
    let dx = x2 - x1
    let dy = y2 - y1
    let d = max (abs dx) (abs dy)
    let dx = dx / d
    let dy = dy / d

    let r =
        seq {
            for i in 0..d do
                yield x1 + i * dx, y1 + i * dy
        }

    r

let fillGridByFlood grid =
    let rec flood toVisit visited walls (minX, minY, maxX, maxY) =
        match toVisit with
        | [] -> visited
        | p :: toVisit ->
            let v = Set.add p visited

            let nextPs =
                dirs
                |> List.map (fun dir -> posPlus p dir)
                |> List.filter (fun (x, y) -> x >= minX && y >= minY && x <= maxX + 1 && y <= maxY + 1)
                |> List.filter (fun x -> Set.contains x walls |> not)

            let nextPs = nextPs |> List.filter (fun x -> not (Set.contains x v))
            flood (nextPs @ toVisit) v walls (minX, minY, maxX, maxY)

    let maxX = grid |> Set.map fst |> Seq.max
    let maxY = grid |> Set.map snd |> Seq.max
    let minX = grid |> Set.map fst |> Seq.min
    let minY = grid |> Set.map snd |> Seq.min

    let start = (minX - 1, minY - 1)

    let allIndexes =
        seq {
            for i in minX - 1 .. maxX + 1 do
                for j in minY - 1 .. maxY + 1 do
                    yield i, j
        }
        |> set

    let filledGrid =
        allIndexes
        - flood [ start ] Set.empty grid (minX - 1, minY - 1, maxX + 1, maxY + 1)

    filledGrid

let fillGridByRemapedIntervals intervals =
    let gridMap = gridMapFromIntervals intervals

    // we use trick, that points on even coordinates represent "big" square and others represent walls for flood fill
    let grid =
        intervals
        |> Seq.collect (fun (p1, p2) -> pointsToLine (posMult gridMap[p1] 2) (posMult gridMap[p2] 2))
        |> set

    let filled = fillGridByFlood grid
    let gridMapRev = gridMap |> Map.toSeq |> Seq.map (fun (k, v) -> v, k) |> Map.ofSeq

    let gridSize (a, b) =
        if a % 2 = 0 || b % 2 = 0 then
            0L
        else
            let x = a / 2
            let y = b / 2

            int64 (fst gridMapRev.[x + 1, y] - fst gridMapRev.[x, y])
            * int64 (snd gridMapRev.[x, y + 1] - snd gridMapRev.[x, y])

    let insides = filled |> Set.map gridSize |> Seq.sum

    let borders =
        (intervals |> Seq.map (fun (a, b) -> posDist a b |> int64) |> Seq.sum) / 2L + 1L

    insides + borders

let part1 () =
    fillGridByFlood (grid true) |> Set.count

let part2 () =
    fillGridByRemapedIntervals (intervals false)

printfn $"{part1 ()}"
printfn $"{part2 ()}"
