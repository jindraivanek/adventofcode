module day06
open Common

type State = { Floor: Set<int * int>; Guard: int * int; GuardDir: int; MapSize: int * int; Visited: Set<int * int>; VisitedRoute: Set<(int * int) * int>; LoopObstacles: Set<int * int> }

let init (lines: string[]) =
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> c, (i, j))) |> Seq.collect id
    //let o = m |> Map.filter (fun c _ -> c = '#') |> Map.toSeq |> Seq.map fst |> set
    let e = m |> Seq.filter (fun (c, _) -> c = '.') |> Seq.map snd |> set
    let g = m |> Seq.find (fun (c, _) -> c = '^') |> snd
    printfn "%A" e
    {
        Floor = e |> Set.add g
        Visited = set [g]
        VisitedRoute = set [g, 0]
        Guard = g
        GuardDir = 0
        MapSize = (lines.[0].Length, lines.Length)
        LoopObstacles = set []
    }


let dirs = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let isInMap (x, y) s = x >= 0 && x < fst s.MapSize && y >= 0 && y < snd s.MapSize

let part1 s = 
    let g2 = posPlus s.Guard dirs[s.GuardDir]
    let s2 =
        if s.Floor |> Set.contains g2 |> not then { s with GuardDir = (s.GuardDir + 1) % 4 }
        else { s with Guard = g2; Visited = Set.add g2 s.Visited }
    if isInMap g2 s2 then 
        printfn "%A %A" s.Guard dirs[s.GuardDir]
        Some s2 else None

let part1Sol = {
    Init = init
    Step = part1
    Result = (fun s -> s.Visited |> Set.count |> string)
}

let part2 s = 
    let g2 = posPlus s.Guard dirs[s.GuardDir]
    let d = (s.GuardDir + 1) % 4
    let s2 =
        if s.Floor |> Set.contains g2 |> not then 
            { s with GuardDir = d }
        else 
            let g3 = posPlus s.Guard dirs[d]
            let o = 
                if s.VisitedRoute |> Set.contains (g3, d) then 
                    printfn "%A"
                    Set.add g2 s.LoopObstacles 
                else s.LoopObstacles
            { s with Guard = g2; Visited = Set.add g2 s.Visited; VisitedRoute = Set.add (g2, s.GuardDir) s.VisitedRoute; LoopObstacles = o }
    if isInMap g2 s2 then 
        //printfn "%A %A" s.Guard dirs[s.GuardDir]
        Some s2 else None

let part2Sol = {
    Init = init
    Step = part2
    Result = (fun s -> s.LoopObstacles |> Set.count |> string)
}

let sol = {
    Day = 6
    Part1 = part1Sol
    Part2 = part2Sol
}
