module day13
open Common

let parseInput (lines: string[]) =
    lines |> splitBy ((=) "") |> Seq.toList |> List.map (fun ls ->
        match ls[0], ls[1], ls[2] with
        | Match "Button A: X\+([0-9]+), Y\+([0-9]+)" [ ax; ay ],
          Match "Button B: X\+([0-9]+), Y\+([0-9]+)" [ bx; by ],
          Match "Prize: X=([0-9]+), Y=([0-9]+)" [ x; y ]
          -> (int64 ax, int64 ay), (int64 bx, int64 by), (int64 x, int64 y)
    )

let solve (ax, ay) (bx, by) (x, y) =
    let A = ax
    let B = bx
    let C = ay
    let D = by
    let b = (-A*y + C*x) / (C*B - A*D)
    let a = (x - b*B) / A
    let tx = a * ax + b * bx
    let ty = a * ay + b * by
    //printfn "%A" (a, b, tx, ty, x, y)
    if x = tx && y = ty then Some (3L*a + b) else None
    
let part1 lines = 
   let xs = parseInput lines
   xs |> List.choose (fun (a, b, t) -> solve a b t) 

let part2 lines = 
   let xs = parseInput lines |> List.map (fun (a, b, (x, y)) -> a, b, (x+10000000000000L, y+10000000000000L))
   xs |> List.choose (fun (a, b, t) -> solve a b t) 

let sol = {
    Day = 13
    Part1 = solution part1 (Seq.sum >> string)
    Part2 = solution part2 (Seq.sum >> string)
}
