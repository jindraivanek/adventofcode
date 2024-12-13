module day13
open Common

let parseInput (lines: string[]) =
    lines |> splitBy ((=) "") |> Seq.toList |> List.map (fun ls ->
        match ls[0], ls[1], ls[2] with
        | Match "Button A: X\+([0-9]+), Y\+([0-9]+)" [ ax; ay ],
          Match "Button B: X\+([0-9]+), Y\+([0-9]+)" [ bx; by ],
          Match "Prize: X=([0-9]+), Y=([0-9]+)" [ x; y ]
          -> (int ax, int ay), (int bx, int by), (int x, int y)
    )

let solve (ax, ay) (bx, by) (x, y) =
    let n = max (max (x / ax) (y / ay)) (max (x / bx) (y / by))
    let n = n + 1
    Seq.allPairs [0..n] [0..n] |> Seq.filter (fun (a,b) ->
        let tx = a * ax + b * bx
        let ty = a * ay + b * by
        //printfn "%A %A %A %A = x=%A y=%A" a b tx ty x y
        x = tx && y = ty
    ) |> Seq.map (fun (a,b) -> printfn "%A %A" a b; 3*a + b) |> Seq.toList |> function | [] -> None | x -> Some (List.min x) 
let part1 lines = 
   let xs = parseInput lines
   xs |> List.choose (fun (a, b, t) -> solve a b t) 

let part2 lines = 
    parseInput lines

let sol = {
    Day = 13
    Part1 = solution part1 (Seq.sum >> string)
    Part2 = solution part2 (Seq.length >> string)
}
