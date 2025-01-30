module day06
open Common

type State = { Floor: Set<int * int>; Wall: Set<int * int>; Guard: int * int; Start: int * int; GuardDir: int; MapSize: int * int; Visited: Set<int * int>; VisitedRoute: Set<(int * int) * int>; LoopObstacles: Set<int * int>; Sim: Set<(int * int) * int>; IsLoop: bool }

let mutable initState = Unchecked.defaultof<State>

let init (lines: string[]) =
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> c, (i, j))) |> Seq.collect id
    let w = m |> Seq.filter (fun (c, _) -> c = '#') |> Seq.map snd |> set
    let e = m |> Seq.filter (fun (c, _) -> c = '.') |> Seq.map snd |> set
    let g = m |> Seq.find (fun (c, _) -> c = '^') |> snd
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
        Some s2 else None

let part1Sol = {
    Init = init
    Step = fun _ -> part1
    Result = (fun s -> s.Visited |> Set.count |> string)
}

let isInLoop s =
    let s2 = step s
    s.VisitedRoute |> Set.contains (s2.Guard, s2.GuardDir)

let runFinishCond init cond step = 
    let mutable finish = false
    run init (fun _ s -> step s |> fun s -> if finish then None elif cond s then finish <- true; Some s else Some s)

let sim = memoize <| fun extraWall -> runFinishCond { initState with Wall = Set.add extraWall initState.Wall} (fun s -> isInLoop s || not(isInMap s)) step |> Seq.toList

let part2 s = 
    let s2 = step s
    let s2 =
        if s.Guard <> s2.Guard then
            let sim = sim (posPlus s.Guard dirs[s.GuardDir])
            let o =
                if isInLoop (sim |> Seq.last) then 
                    { s2 with LoopObstacles = Set.add s2.Guard s.LoopObstacles; IsLoop = true }
                else { s2 with IsLoop = false }
            { o with Sim = sim |> List.map (fun s -> s.Guard, s.GuardDir) |> Set.ofList }
        else s2
    if isInMap s2 then 
        Some s2 else None

let part2Sol = {
    Init = init
    Step = fun _ ->part2
    Result = (fun s -> s.LoopObstacles |> Set.filter ((<>) s.Start) |> Set.count |> string)
}

let sol = {
    Day = 6
    Part1 = part1Sol
    Part2 = part2Sol
}
