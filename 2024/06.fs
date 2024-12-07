module day06
open Common

type State = { Floor: Set<int * int>; Wall: Set<int * int>; Guard: int * int; Start: int * int; GuardDir: int; MapSize: int * int; Visited: Set<int * int>; VisitedRoute: Set<(int * int) * int>; LoopObstacles: Set<int * int>; Sim: Set<(int * int) * int>; IsLoop: bool }

let mutable initState = Unchecked.defaultof<State>

let init (lines: string[]) =
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> c, (i, j))) |> Seq.collect id
    let w = m |> Seq.filter (fun (c, _) -> c = '#') |> Seq.map snd |> set
    let e = m |> Seq.filter (fun (c, _) -> c = '.') |> Seq.map snd |> set
    let g = m |> Seq.find (fun (c, _) -> c = '^') |> snd
    printfn "%A" e
    printfn "%A" g
    let s =
        {
            Floor = e |> Set.add g
            Wall = w
            Visited = set [g]
            VisitedRoute = set [g, 0]
            Start = g
            Guard = g
            GuardDir = 0
            MapSize = (lines.[0].Length, lines.Length)
            LoopObstacles = set []
            Sim = Set.empty
            IsLoop = false
        }
    initState <- s
    s


let dirs = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let isInMap' (x, y) s = x >= 0 && x < fst s.MapSize && y >= 0 && y < snd s.MapSize
let isInMap s = isInMap' s.Guard s

let turnRight s = { s with GuardDir = (s.GuardDir + 1) % 4 }

let step s =
    let g2 = posPlus s.Guard dirs[s.GuardDir]
    let s2 =
        if s.Wall |> Set.contains g2 then turnRight s
        else { s with 
                Guard = g2
                Visited = Set.add g2 s.Visited
                 
                }
    { s2 with VisitedRoute = Set.add (s2.Guard, s2.GuardDir) s.VisitedRoute }

let part1 s = 
    let s2 = step s
    if isInMap s2 then 
        //printfn "%A %A" s.Guard dirs[s.GuardDir]
        Some s2 else None

let part1Sol = {
    Init = init
    Step = part1
    Result = (fun s -> s.Visited |> Set.count |> string)
}

// let step s =
//     let g2 = posPlus s.Guard dirs[s.GuardDir]
//     let d = (s.GuardDir + 1) % 4
//     if s.Floor |> Set.contains g2 |> not then 
//         { s with GuardDir = d }
//     else 
//         let g3 = posPlus s.Guard dirs[d]
//         let o = 
//             if s.VisitedRoute |> Set.contains (g3, d) then 
//                 printfn "%A" g2
//                 Set.add g2 s.LoopObstacles 
//             else s.LoopObstacles
//         { s with Guard = g2; Visited = Set.add g2 s.Visited; VisitedRoute = Set.add (g2, s.GuardDir) s.VisitedRoute; LoopObstacles = o }

let isInLoop s =
    let s2 = step s
    //let lineLoop = s.VisitedRoute |> Seq.groupBy fst |> Seq.exists (fun (g, xs) -> (xs |> Seq.map snd |> Seq.sort |> Seq.toList) |> fun x -> x = [0;2] || x = [1;3])
    s.VisitedRoute |> Set.contains (s2.Guard, s2.GuardDir)
let isInLoopByHistory xs =
    xs |> Seq.fold (fun (s,b) x -> if b then (s, b) elif Set.contains x s then (s, true) else Set.add x s, false) (Set.empty, false)
    |> snd

let runFinishCond init cond step = 
    let mutable finish = false
    run init (step >> fun s -> if finish then None elif cond s then finish <- true; Some s else Some s)

let part2 s = 
    let s2 = step s
    let s2 =
        if s.Guard <> s2.Guard then
            //let sim = runFinishCond ({ s with Wall = Set.add (posPlus s.Guard dirs[s.GuardDir]) s.Wall}) (fun s -> isInLoop s || not(isInMap s)) step |> Seq.toList
            let sim = runFinishCond ({ initState with Wall = Set.add (posPlus s.Guard dirs[s.GuardDir]) s.Wall}) (fun s -> isInLoop s || not(isInMap s)) step |> Seq.toList
            let o =
                if isInLoop (sim |> Seq.last) then 
                    printfn "O %A" s2.Guard
                    { s2 with LoopObstacles = Set.add s2.Guard s.LoopObstacles; IsLoop = true }
                else { s2 with IsLoop = false }
            { o with Sim = sim |> List.map (fun s -> s.Guard, s.GuardDir) |> Set.ofList }
        else s2
    if isInMap s2 then 
        //printfn "%A %A" s2.Guard dirs[s2.GuardDir]
        Some s2 else None

let part2Sol = {
    Init = init
    Step = part2
    Result = (fun s -> s.LoopObstacles |> Set.filter ((<>) s.Start) |> Set.count |> string)
}

let sol = {
    Day = 6
    Part1 = part1Sol // 4433
    Part2 = part2Sol // wrong: 1584, 1585, 1586
}
