#time

let lines = System.IO.File.ReadAllLines("input")
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
let posMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let dirNeg (x, y) = (-x, -y)
let dirsMap = [ 'U', (0, -1); 'L', (-1, 0); 'D', (0, 1); 'R', (1, 0) ] |> Map.ofSeq
let dirs = dirsMap |> Map.toList |> List.map snd

let parsed =
    lines |> Seq.map (fun s -> let [| d; x; c |] = s.Split(' ') in Seq.head d, int x, c)

let instr = parsed |> Seq.map (fun (d, x, c) -> Seq.init x (fun _ -> Map.find d dirsMap)) |> Seq.concat
printfn $"%A{parsed}"
printfn $"%A{instr}"
let grid =
    instr |> Seq.fold (fun (s, p) d -> Set.add p s, posPlus p d) (Set.empty, (0, 0)) |> fst
        
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

//let start = seq{ for i in minX+1..maxX-1 do for j in minY+1..maxY-1 do yield i, j } |> Seq.filter (fun p -> Set.contains p grid |> not) |> Seq.head
let start = (minX-1,minY-1)

let allIndexes = seq{ for i in minX-1..maxX+1 do for j in minY-1..maxY+1 do yield i, j } |> set
let filledGrid = allIndexes - flood [ start ] Set.empty grid (minX-1, minY-1, maxX+1, maxY+1)

printGridSet filledGrid

let part1 = filledGrid |> Set.count
let part2 = 0

printfn $"{part1}"
printfn $"{part2}"
