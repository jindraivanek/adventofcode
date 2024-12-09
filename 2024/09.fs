module day09
open Common

let parseInput (lines: string[]) =
    lines |> Array.map (fun s -> s |> Seq.map (string >> int) |> Seq.indexed |> Seq.collect (fun (i, x) -> Seq.replicate x (if i%2 = 1 then None else (Some (i/2)))) |> Seq.toList)
    |> Seq.head

let moveBLocks xs =
    let swap i j (arr: _[]) =
        //printfn "swap %i %i %A %A" i j arr[i] arr[j]
        let tmp = arr[i]
        arr[i] <- arr[j]
        arr[j] <- tmp
    let rs = xs |> List.indexed |> List.rev
    let arr = xs |> List.toArray
    let rec loop i rs =
        //printfn "%A" arr
        if i >= arr.Length then arr |> List.ofArray else
        match arr[i], rs with
        | _, [] -> arr |> List.ofArray
        | _, (j, _):: _ when i >= j -> arr |> List.ofArray
        | _, (_, None)::rs -> loop i rs
        | Some _, rs -> loop (i+1) rs
        | None, (j, Some r)::rs -> swap i j arr; loop (i+1) rs
    loop 0 rs
        
let part1 lines = 
   let xs = parseInput lines |> moveBLocks
   xs |> Seq.mapi (fun i x -> x |> Option.defaultValue 0 |> (*) i |> int64)
   |> Seq.sum

let part2 lines = 
    parseInput lines

let sol = {
    Day = 9
    Part1 = solution part1 (string)
    Part2 = solution part2 (Seq.length >> string)
}
