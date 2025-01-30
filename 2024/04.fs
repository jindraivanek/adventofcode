module day04

open Common

let parseToGrid (lines: string[]) =
    lines
    |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> (i, j), c))
    |> Seq.collect id
    |> Map.ofSeq

let dirs = [ (0, -1); (-1, 0); (0, 1); (1, 0); (-1, -1); (-1, 1); (1, -1); (1, 1) ]
let posPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let posMul (x, y) c = (x * c, y * c)

let variants n grid =
    seq {
        for d in dirs do
            for p in grid |> Map.keys do
                let xs = [ 0 .. n - 1 ] |> Seq.map (fun i -> posPlus p (posMul d i))

                if xs |> Seq.forall (fun p -> Map.containsKey p grid) then
                    yield xs
    }

let part1 lines =
    let grid = parseToGrid lines

    grid
    |> variants 4
    |> Seq.map (Seq.map (fun p -> string grid[p]) >> String.concat "")
    |> Seq.filter ((=) "XMAS")

let variantsMask posList grid =
    seq {
        for p in grid |> Map.keys do
            let xs = posList |> Seq.map (fun u -> posPlus p u)

            if xs |> Seq.forall (fun p -> Map.containsKey p grid) then
                yield xs
    }

let part2 lines =
    let grid = parseToGrid lines
    let xMask = [ 0, 0; 1, 1; 2, 2; 2, 0; 1, 1; 0, 2 ]

    grid
    |> variantsMask xMask
    |> Seq.map (Seq.map (fun p -> string grid[p]) >> String.concat "")
    |> Seq.filter (fun s -> (s[0..2] = "MAS" || s[0..2] = "SAM") && (s[3..5] = "MAS" || s[3..5] = "SAM"))

let sol =
    { Day = 4
      Part1 = solution part1 (Seq.length >> string)
      Part2 = solution part2 (Seq.length >> string) }
