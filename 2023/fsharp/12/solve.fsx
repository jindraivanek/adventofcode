#time

let teePrint x =
    printfn $"{x}"
    x

//let lines = System.IO.File.ReadAllLines("input")
let lines = System.IO.File.ReadAllLines("sample")

let parsed =
    lines
    |> Seq.map (fun s -> 
        let [| xs1; xs2 |] = s.Split(' ')
        let counts = xs2.Split(',') |> Seq.map int |> Seq.toList
        xs1, counts)
    |> Seq.toList

let rec springs xs cs cRem =
    printfn $"%A{xs} %A{cs} %A{cRem}"
    match xs, cs, cRem with
    | [], [], None -> 1
    | '.'::xs, cs, None -> springs xs cs None
    | '#'::xs, cs, Some 1 
    | '?'::xs, cs, Some 1 
        -> springs xs cs None
    | '#'::xs, cs, Some c
    | '?'::xs, cs, Some c
        -> springs xs cs (Some (c-1))
    | '#'::xs, (1::cs), None -> 
        springs xs cs None
    | '#'::xs, (c::cs), None -> 
        springs xs cs (Some (c-1))
    | '?'::xs, (1::cs), None -> 
        springs xs cs None + springs xs (1::cs) None
    | '?'::xs, (c::cs), None -> 
        springs xs cs (Some (c-1)) + springs xs (c::cs) None
    | _ -> 0

let part1 = parsed |> Seq.map (fun (xs, cs) -> springs (xs |> Seq.toList) cs None |> teePrint) |> Seq.sum
let part2 = 0

printfn $"{part1}"
printfn $"{part2}"
