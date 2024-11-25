#time

let lines = System.IO.File.ReadAllLines("__SOURCE_DIRECTORY__/../../../../input/2023/10.txt")
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
let posNeg (x, y) = (-x, -y)
let posMinus p1 p2 = posPlus p1 (posNeg p2)
let L = (-1, 0)
let R = (1, 0)
let U = (0, -1)
let D = (0, 1)
let dirs = [ L; R; U; D ]

let rotateLeft =
    function
    | p when p = R -> U
    | p when p = U -> L
    | p when p = L -> D
    | p when p = D -> R
    | p -> failwith $"rotateLeft: {p}"

let rotateRight = rotateLeft >> rotateLeft >> rotateLeft

let indexed =
    lines
    |> Seq.mapi (fun i s -> s |> Seq.mapi (fun j c -> (j, i), c))
    |> Seq.collect id

let indexedMap = indexed |> Map.ofSeq

let pipesMap =
    [ '|', [ D; U ]
      '-', [ R; L ]
      'L', [ R; U ]
      'J', [ U; L ]
      '7', [ L; D ]
      'F', [ D; R ] ]
    |> Map.ofSeq

let pipes =
    indexed
    |> Seq.choose (fun (p, c) ->
        let dirs =
            match c with
            | '|'
            | '-'
            | 'L'
            | 'J'
            | '7'
            | 'F' -> Some pipesMap.[c]
            | _ -> None

        dirs |> Option.map (fun dirs -> p, dirs))
    |> Map.ofSeq

let pipeVariants = pipesMap |> Map.toList
let startPos = indexed |> Seq.find (fun (_, c) -> c = 'S') |> fst
let maxX = indexed |> Seq.map fst |> Seq.maxBy fst |> fst
let maxY = indexed |> Seq.map fst |> Seq.maxBy snd |> snd
let allIndexes = indexed |> Seq.map fst |> set

let rec findCycle pipes startPos acc visited i p =
    let v = Set.add p visited
    let nAcc = (p, i) :: acc

    match Map.tryFind p pipes with
    | Some dirs ->
        let nextPs = dirs |> List.map (fun dir -> posPlus p dir)

        if nextPs |> List.contains startPos && i > 1 then
            Some nAcc
        else
            match nextPs |> List.tryFind (fun x -> not (Set.contains x v)) with
            | Some nextP -> findCycle pipes startPos nAcc v (i + 1) nextP
            | None -> None
    | None -> None

let cycleDists =
    pipeVariants
    |> List.map (fun (pipeChar, v) -> pipeChar, Map.add startPos v pipes)
    |> List.choose (fun (pipeChar, pipes) ->
        match findCycle pipes startPos [] (set [ startPos ]) 0 startPos with
        | Some xs ->
            let revXs = xs |> List.mapi (fun i (p, _) -> p, i + 1)

            Some(
                pipeChar,
                List.map fst xs,
                [ xs; revXs ]
                |> List.collect id
                |> List.groupBy fst
                |> List.map (fun (p, ps) -> p, ps |> List.map snd |> List.min)
            )
        | None -> None)

let startPipe, mainLoop, mainLoopDist =
    cycleDists |> List.maxBy ((fun (_, _, x) -> x) >> List.map snd >> List.max)

let maxDist = mainLoopDist |> List.map snd |> List.max
let part1 = maxDist

printfn $"{part1}"

let toExt (i, j) = i * 2 + 1, j * 2 + 1
let fromExt (i, j) = (i - 1) / 2, (j - 1) / 2

let rec pipeExt (i, j) =
    let (x, y) = toExt (i, j)

    function
    | 'S' -> pipeExt (i, j) startPipe
    | '|' -> [ x, y - 1; x, y; x, y + 1 ]
    | '-' -> [ x - 1, y; x, y; x + 1, y ]
    | 'L' -> [ x, y - 1; x, y; x + 1, y ]
    | 'J' -> [ x, y - 1; x, y; x - 1, y ]
    | '7' -> [ x, y + 1; x, y; x - 1, y ]
    | 'F' -> [ x, y + 1; x, y; x + 1, y ]

let walls =
    mainLoop
    |> List.map (fun p -> p, Map.find p indexedMap)
    |> List.collect (fun (p, c) -> pipeExt p c)
    |> set

let rec flood toVisit visited walls (maxX, maxY) =
    match toVisit with
    | [] -> visited
    | p :: toVisit ->
        let v = Set.add p visited

        let nextPs =
            dirs
            |> List.map (fun dir -> posPlus p dir)
            |> List.filter (fun (x, y) -> x >= -1 && y >= -1 && x <= maxX + 1 && y <= maxY + 1)
            |> List.filter (fun x -> Set.contains x walls |> not)

        let nextPs = nextPs |> List.filter (fun x -> not (Set.contains x v))
        flood (nextPs @ toVisit) v walls (maxX, maxY)

let outsideExt = flood [ -1, -1 ] Set.empty walls (toExt (maxX, maxY))
let outside = outsideExt |> Set.map fromExt
let cycleSet = mainLoop |> set
let enclosed = allIndexes - outside - cycleSet

// printGridSet outsideExt
let part2 = enclosed |> Set.count

printfn $"{part2}"
