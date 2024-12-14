module day14

open Common

let (%) x y = ((x % y) + y) % y

let posPlusMod (w, h) (x1, y1) (x2, y2) = (x1 + x2) % w, (y1 + y2) % h

type State =
    { Robots: list<(int * int) * (int * int)>
      Size: int * int }

let parseInput (lines: string[]) =
    let robots =
        lines
        |> Seq.map (function
            | Match "p=([0-9]+),([0-9]+) v=([-]?[0-9]+),([-]?[0-9]+)" [ px; py; vx; vy ] ->
                (int px, int py), (int vx, int vy))
        |> Seq.toList

    let size = if Seq.length robots > 15 then (101, 103) else (11, 7)
    { Robots = robots; Size = size }

let quadrant (w, h) (x, y) = sign (x - (w / 2)), sign (y - (h / 2))

let quadrantCounts (s: State) =
    s.Robots
    |> Seq.map fst
    |> Seq.countBy (quadrant s.Size)
    |> Seq.toList

let quadrantScore s = s |> quadrantCounts |> Seq.filter (fun ((q1, q2), _) -> q1 * q2 <> 0) |> Seq.map (snd >> int64) |> Seq.reduce (*)
let step i (s: State) =
    //printfn "%A" (i, (s.Robots |> List.map id), quadrantCounts s)
    //printfn "%A" (i, quadrantScore s)

    { s with
        Robots = s.Robots |> List.map (fun (p, v) -> posPlusMod s.Size p v, v) }

let stepUntilCond f i s =
    if f i s then None else Some(step i s)


let part1 n =
    { Init = parseInput
      Step = stepUntilCond (fun i _ -> i >= n)
      Result = (fun s -> quadrantScore s |> string) }

let part2 = part1 10403
let sol =
    { Day = 14
      Part1 = part1 100
      Part2 = solution part2.Init (fun x -> run x part2.Step |> Seq.map quadrantScore |> Seq.indexed |> Seq.minBy snd |> string) }
