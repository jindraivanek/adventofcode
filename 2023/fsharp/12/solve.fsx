#time

let lines = System.IO.File.ReadAllLines("__SOURCE_DIRECTORY__/../../../../input/2023/12.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let teePrint x =
    printfn $"%A{x}"
    x

let memoizeRec f =
    let cache = System.Collections.Generic.Dictionary<_, _>()

    let rec f' x =
        match cache.TryGetValue x with
        | true, v -> v
        | _ ->
            let v = f f' x
            cache.Add(x, v)
            v

    f'

let chunkBy f xs =
    let rec go acc xs =
        match xs with
        | [] -> List.map List.rev acc |> List.rev |> List.filter ((<>) [])
        | x :: xs ->
            match acc with
            | [] -> go [ [ x ] ] xs
            | (y :: ys) :: acc when f x <> f y -> go ([ x ] :: (y :: ys) :: acc) xs
            | ys :: acc -> go ((x :: ys) :: acc) xs

    go [] xs

let pairCombNum xs ys =
    let n = xs |> List.last |> (fun (i, x) -> i + List.length x + 1)
    let ySum ys = List.sum ys + List.length ys - 1

    let rec pairComb' =
        memoizeRec
        <| fun recf (xs, ys) ->
            let recf x y = recf (x, y)
            //printfn $"%A{acc} %A{xs} %A{ys}"
            let hashPrefix xs =
                xs |> List.takeWhile ((=) '#') |> List.length

            match xs, ys with
            | gs, [] when gs |> List.exists (fun (_, g) -> g |> List.contains '#') -> 0L
            | _, [] -> 1L
            | [], _ -> 0L
            | (_, []) :: xs, ys -> recf xs ys
            | (i, _) :: _, ys when n - i + 1 < ySum ys -> 0
            | (i, g) :: xs, y :: ys ->
                let lg = List.length g
                let hashPrefix = hashPrefix g
                let validBlock = (if y > lg then None else List.skip y g |> List.tryHead) = Some '?'

                match (i, g) :: xs, y :: ys with
                | (i, (('#' :: _) as g)) :: xs, y :: ys when lg > y + 1 && validBlock && y >= hashPrefix ->
                    recf ((i + y + 1, List.skip (y + 1) g) :: xs) (ys)
                | (i, (('#' :: _) as g)) :: xs, y :: ys when lg = y || (lg = y + 1 && validBlock) -> recf xs ys
                | (i, (('#' :: _) as g)) :: _, _ -> 0
                | (i, g) :: xs, y :: ys when lg > y + 1 && validBlock ->
                    recf ((i + 1, List.skip 1 g) :: xs) (y :: ys)
                    + recf ((i + y + 1, List.skip (y + 1) g) :: xs) (ys)
                | (i, g) :: xs, y :: ys when lg = y || (lg = y + 1 && validBlock) ->
                    recf ((i + 1, List.skip 1 g) :: xs) (y :: ys) + recf xs ys
                | (i, g) :: xs, ys -> recf ((i + 1, List.skip 1 g) :: xs) ys

    pairComb' (xs, ys)

let parsed =
    lines
    |> Seq.map (fun s ->
        let [| xs1; xs2 |] = s.Split(' ')
        let counts = xs2.Split(',') |> Seq.map int |> Seq.toList
        xs1, counts)
    |> Seq.toList

let groups xs =
    xs
    |> Seq.indexed
    |> Seq.toList
    |> chunkBy (fun (_, c) -> c = '.')
    |> List.filter (fun xs -> (List.head >> snd) xs <> '.')
    |> List.map (fun xs -> xs |> List.map fst |> List.min, xs |> List.map snd)

let lineCombNum xs cs =
    let xs = xs |> Seq.toList
    let g = groups xs //|> List.map teePrint
    let r = pairCombNum g cs //|> teePrint
    r //|> teePrint


let unfold (xs: string, cs) =
    let xs = Seq.replicate 5 xs |> String.concat "?"
    let cs = cs |> List.replicate 5 |> List.concat
    xs, cs

let part1 () =
    parsed |> Seq.map (fun (xs, cs) -> lineCombNum xs cs) |> Seq.sum

printfn $"{part1 ()}"

let part2 () =
    parsed
    |> Seq.map unfold
    |> Seq.map (fun (xs, cs) -> lineCombNum xs cs |> int64)
    |> Seq.sum

printfn $"{part2 ()}"
