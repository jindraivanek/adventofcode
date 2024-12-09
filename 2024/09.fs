module day09
open Common

let parseInput (lines: string[]) =
    lines |> Array.map (fun s -> s |> Seq.map int |> Seq.indexed |> Seq.collect (fun (i, x) -> Seq.replicate x (if i%2 = 0 then None else (Some (i/2)))) |> Seq.toList)
    |> Seq.head

let moveBLocks xs =
    let r = xs |> List.rev
    let rec loop acc r xs =
        match r, xs with
        | [], [] -> List.rev acc
        | (None)::xs, [] -> loop acc [] xs
        | (Some x)::xs, [] -> loop (x::acc) [] xs
        | (Some a)::r, None::xs -> loop (a::acc) r xs
        | None::r, (Some x)::xs -> loop (x::acc) r xs
        | (Some a)::rs, (Some x)::xs -> loop (x::acc) r xs
        | _ -> failwith "FAIL"
    loop [] r xs
        
let part1 lines = 
   parseInput lines
   |> moveBLocks
   |> Seq.map (printfn "%A")

let part2 lines = 
    parseInput lines

let sol = {
    Day = 9
    Part1 = solution part1 (Seq.length >> string)
    Part2 = solution part2 (Seq.length >> string)
}
