#time

let teePrint x =
    printfn $"%A{x}"
    x

let chunkBy f xs =
    let rec go acc xs =
        match xs with
        | [] -> List.map List.rev acc |> List.rev |> List.filter ((<>) [])
        | x::xs ->
            match acc with
            | [] -> go [[x]] xs
            | (y::ys)::acc when f x <> f y -> go ([x]::(y::ys)::acc) xs
            | ys::acc -> go ((x::ys)::acc) xs
    go [] xs

let pairComb xs ys = 
    let rec pairComb' acc xs ys =
        //printfn $"%A{acc} %A{xs} %A{ys}"
        let hashPrefix xs = xs |> List.takeWhile ((=) '#') |> List.length
        match xs, ys with
        | gs, [] when gs |> List.exists (fun (_, g) -> g |> List.contains '#') -> []
        | _, [] -> acc
        | [], _ -> []
        | (_,[])::xs, ys -> pairComb' acc xs ys
        | (i,g)::xs, y::ys ->
            let lg = List.length g
            let hashPrefix = hashPrefix g
            let validBlock = (if y > lg then None else List.skip y g |> List.tryHead) = Some '?'
            match (i,g)::xs, y::ys with
            | (i, (('#'::_) as g))::xs, y::ys when lg > y + 1 && validBlock && y >= hashPrefix -> 
                let acc' = acc |> List.map (fun zs -> ((i,y)::zs))
                pairComb' acc' ((i+y+1, List.skip (y+1) g) :: xs) (ys)
            | (i, (('#'::_) as g))::xs, y::ys when lg = y || (lg = y + 1 && validBlock) ->
                let acc' = acc |> List.map (fun zs -> ((i,y)::zs))
                pairComb' acc' xs ys
            | (i, (('#'::_) as g))::_, _ -> []
            | (i, g)::xs, y::ys when lg > y + 1 && validBlock -> 
                let acc' = acc |> List.map (fun zs -> ((i,y)::zs))
                pairComb' acc ((i+1, List.skip 1 g) :: xs) (y::ys) @ pairComb' acc' ((i+y+1, List.skip (y+1) g) :: xs) (ys)
            | (i, g)::xs, y::ys when lg = y || (lg = y + 1 && validBlock) ->
                let acc' = acc |> List.map (fun zs -> ((i,y)::zs))
                pairComb' acc ((i+1, List.skip 1 g) :: xs) (y::ys) @ pairComb' acc' xs ys
            | (i,g)::xs, ys ->
                pairComb' acc ((i+1, List.skip 1 g) :: xs) ys
    pairComb' [[]] xs ys |> List.filter ((<>) [])

let pairCombNum xs ys = 
    let n = xs |> List.last |> (fun (i, x) -> i + List.length x + 1)
    let ySum ys = List.sum ys + List.length ys - 1
    let rec pairComb' acc xs ys =
        //printfn $"%A{acc} %A{xs} %A{ys}"
        let hashPrefix xs = xs |> List.takeWhile ((=) '#') |> List.length
        match xs, ys with
        | gs, [] when gs |> List.exists (fun (_, g) -> g |> List.contains '#') -> 0
        | _, [] -> acc
        | [], _ -> 0
        | (_,[])::xs, ys -> pairComb' acc xs ys
        | (i,_)::_, ys when n-i+1 < ySum ys -> 0
        | (i,g)::xs, y::ys ->
            let lg = List.length g
            let hashPrefix = hashPrefix g
            let validBlock = (if y > lg then None else List.skip y g |> List.tryHead) = Some '?'
            match (i,g)::xs, y::ys with
            | (i, (('#'::_) as g))::xs, y::ys when lg > y + 1 && validBlock && y >= hashPrefix -> 
                pairComb' acc ((i+y+1, List.skip (y+1) g) :: xs) (ys)
            | (i, (('#'::_) as g))::xs, y::ys when lg = y || (lg = y + 1 && validBlock) ->
                pairComb' acc xs ys
            | (i, (('#'::_) as g))::_, _ -> 0
            | (i, g)::xs, y::ys when lg > y + 1 && validBlock -> 
                pairComb' acc ((i+1, List.skip 1 g) :: xs) (y::ys) + pairComb' acc ((i+y+1, List.skip (y+1) g) :: xs) (ys)
            | (i, g)::xs, y::ys when lg = y || (lg = y + 1 && validBlock) ->
                pairComb' acc ((i+1, List.skip 1 g) :: xs) (y::ys) + pairComb' acc xs ys
            | (i,g)::xs, ys ->
                pairComb' acc ((i+1, List.skip 1 g) :: xs) ys
    pairComb' 1 xs ys


let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

let parsed =
    lines
    |> Seq.map (fun s -> 
        let [| xs1; xs2 |] = s.Split(' ')
        let counts = xs2.Split(',') |> Seq.map int |> Seq.toList
        xs1, counts)
    |> Seq.toList

let groups xs =
    xs |> Seq.indexed |> Seq.toList |> chunkBy (fun (_, c) -> c='.') |> List.filter (fun xs -> (List.head >> snd) xs <> '.')
    |> List.map (fun xs -> xs |> List.map fst |> List.min, xs |> List.map snd)

let lineComb xs cs =
    let xs = xs |> Seq.toList
    let g = groups xs //|> List.map teePrint
    let r = pairComb g cs //|> teePrint
    // r |> Seq.iter (fun rs ->
    //     let idx = rs |> Seq.collect (fun (i,n) -> [i .. i+n-1]) |> set
    //     let s = xs |> Seq.mapi (fun i c -> if idx.Contains i then '#' else '.') |> Seq.toList //|> teePrint
    //     List.zip xs s |> List.iter (function | ('.', '#') | ('#', '.') -> printfn $"%A{xs}"; failwith "mismatch" | _ -> ())
    // )
    r |> Seq.length |> teePrint

let lineCombNum xs cs =
    let xs = xs |> Seq.toList
    let g = groups xs //|> List.map teePrint
    let r = pairCombNum g cs //|> teePrint
    // r |> Seq.iter (fun rs ->
    //     let idx = rs |> Seq.collect (fun (i,n) -> [i .. i+n-1]) |> set
    //     let s = xs |> Seq.mapi (fun i c -> if idx.Contains i then '#' else '.') |> Seq.toList //|> teePrint
    //     List.zip xs s |> List.iter (function | ('.', '#') | ('#', '.') -> printfn $"%A{xs}"; failwith "mismatch" | _ -> ())
    // )
    r |> teePrint


let unfold (xs: string, cs) =
    let xs = xs |> Seq.map (fun c -> String.replicate 5 (string c)) |> String.concat "?"
    let cs = cs |> List.collect (fun x -> List.replicate 5 x)
    xs, cs
let part1() = parsed |> Seq.map teePrint |> Seq.map (fun (xs, cs) -> lineCombNum xs cs) |> Seq.sum
printfn $"{part1()}" //7191
let part2() = parsed |> Seq.map unfold |> Seq.map teePrint |> Seq.map (fun (xs, cs) -> lineCombNum xs cs |> int64) |> Seq.sum

printfn $"{part2()}"
