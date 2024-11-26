let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2022/01.txt")
let calories = 
    ((0, []), lines) 
    ||> Seq.fold (fun (cur, acc) s -> 
        if s = "" then 
            0, cur :: acc
        else
            let x = int s
            cur + x, acc
    ) |> snd
let part1 = calories |> List.sortDescending |> List.head
let part2 = calories |> List.sortDescending |> List.take 3 |> List.sum

printfn $"{part1}"
printfn $"{part2}"