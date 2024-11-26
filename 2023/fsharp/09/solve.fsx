#time

let teePrint x =
    printfn $"%A{x}"
    x

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/09.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let sequences = lines |> Array.map (fun l -> l.Split(' ') |> Array.map int64)

let predict isPart2 xs =
    let rec predict' ys =
        match ys with
        | [ x ] -> x
        | _ when List.forall ((=) 0L) ys -> 0L
        | _ ->
            let d = ys |> List.pairwise |> List.map (fun (a, b) -> b - a)
            List.last ys + predict' d

    predict' (List.ofArray xs |> (if isPart2 then List.rev else id))

let part1 = sequences |> Array.map (predict false) |> Array.sum
let part2 = sequences |> Array.map (predict true) |> Array.sum

printfn $"{part1}"
printfn $"{part2}"
