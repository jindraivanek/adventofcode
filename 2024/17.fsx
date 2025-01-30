let target =
    [ 2; 4; 1; 1; 7; 5; 0; 3; 4; 7; 1; 6; 5; 5; 3; 0 ]
    |> List.map int64
    |> List.toArray

let rec run n acc a =
    let m x = x &&& 7L
    let b1 = m a ^^^ 1L
    let c = a / (1L <<< int32 b1)
    let b2 = ((b1 ^^^ c) ^^^ 6L) |> m

    if b2 = target[n] then
        if n = 0 then
            printfn "%A" (a, b2 :: acc)
        else
            for i in 0L .. 7L do
                run (n - 1) (b2 :: acc) (a * 8L + i)

for a in 1..7 do
    printfn "A=%A" a
    run 15 [] a
