module day21
open Common
open Common.Grid
open Common.Graph

let keypadString = """
789
456
123
#0A
"""

let dirKeypadString = """
#^A
<v>
"""

//let dirs = [ (0, -1); (0, 1); (1, 0); (-1, 0) ]

let dirToChar = Seq.zip dirs "^>v<" |> Map.ofSeq

let fixDirPath (s: string) = 
    let fixPart prev s = 
        let prev = prev |> Option.map string |> Option.defaultValue ""
        let r = Seq.countBy id s |> Seq.sortBy (fun (c, _) -> Seq.findIndex ((=) c) (prev+"^>v<")) |> Seq.map (fun (c, x) -> String.replicate x (string c)) |> String.concat ""
        // printfn "%s %A" prev s
        // printfn "->%A" r
        r
    let r = s.Split('A') |> mapWithState (fun prev b -> (if b = "" then prev else Seq.tryLast b), fixPart prev b) None |> String.concat "A"
    printfn "  %A" s
    printfn "->%A" r
    r

let getGraph (s: string) =
    let lines = s.Split(System.Environment.NewLine)
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> (i, j), c)) |> Seq.collect id |> Seq.filter (fun (_, c) -> c <> '#') |> Map.ofSeq
    let neighF p =
        let moves = dirs |> List.map (posPlus p) |> List.filter (fun v -> Map.containsKey v m)
        moves
    let edges = m |> Map.keys |> Seq.collect (fun p -> neighF p |> Seq.map (fun v -> p, v)) |> Seq.toList
    {| Map = m |> Map.toSeq |> Seq.map (fun (k, v) -> (v, k)) |> Map.ofSeq; Nodes = Map.keys m; Edges = edges |}

let getPathDirs (dists: Map<_,_>) u v =
    let _,p = dists[u, v]
    List.pairwise p |> List.map (fun (a, b) -> posMinus b a) |> List.map (fun d -> dirToChar[d])

let getWalkDirs (dists: Map<_,_>) xs =
    Seq.pairwise xs |> Seq.map (fun (u, v) -> getPathDirs dists u v |> Seq.map string |> String.concat "") |> String.concat "A"
    |> fixDirPath
    |> fun s -> s + "A"


let pathScore p = 
    List.pairwise p |> List.map (fun (a, b) -> posMinus b a) |> List.pairwise |> List.sumBy (fun (a, b) -> if a = b then 0 else 1)
let part1 lines = 
   let dirKeypadG = getGraph dirKeypadString
   let dirKeypadDist = floydWarshallPaths pathScore dirKeypadG.Nodes dirKeypadG.Edges
   let halfPath xs =
    let p2 = xs |> getWalkDirs dirKeypadDist
    let p3 = "A" + p2 |> Seq.map (fun c -> dirKeypadG.Map[c]) |> getWalkDirs dirKeypadDist
    p3.Length
   let dirKeypadDist2 = floydWarshallPaths halfPath dirKeypadG.Nodes dirKeypadG.Edges
   let fullPath xs =
    let p1 = List.pairwise xs |> List.map (fun (a, b) -> posMinus b a) |> List.map (fun d -> string dirToChar[d]) |> String.concat ""
    let p2 = "A" + p1 |> Seq.map (fun c -> dirKeypadG.Map[c]) |> getWalkDirs dirKeypadDist
    let p3 = "A" + p2 |> Seq.map (fun c -> dirKeypadG.Map[c]) |> getWalkDirs dirKeypadDist
    p3.Length 
   let keypadG = getGraph keypadString
   let keypadDist = floydWarshallPaths fullPath keypadG.Nodes keypadG.Edges
   lines |> Seq.map (fun (s: string) ->
    let p1 = "A" + s |> Seq.map (fun c -> keypadG.Map[c]) |> getWalkDirs keypadDist
    let p2 = "A" + p1 |> Seq.map (fun c -> dirKeypadG.Map[c]) |> getWalkDirs dirKeypadDist
    let p3 = "A" + p2 |> Seq.map (fun c -> dirKeypadG.Map[c]) |> getWalkDirs dirKeypadDist2
    printfn "%i %A %A %A %A" p3.Length s p1 p2 p3
    let x = s[.. -1] |> int
    p3.Length * x
   ) |> Seq.toList

let part2 lines = 
    [""]

let sol = {
    Day = 21
    Part1 = solution part1 (Seq.length >> string)
    Part2 = solution part2 (Seq.length >> string)
}
