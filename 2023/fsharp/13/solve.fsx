#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

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

let diffCount xs ys =
    List.zip xs ys |> List.filter (fun (x, y) -> x <> y) |> List.length

let patterns =
    lines |> Seq.toList |> chunkBy ((<>) "") |> List.map (List.map Seq.toList)

let findReflectionWithDiffCount k pattern =
    let sumDiffs xs ys =
        List.zip xs ys |> List.sumBy (fun (x, y) -> diffCount x y)

    let rec go before after =
        let n = min (List.length before) (List.length after)

        if n > 0 && sumDiffs (List.take n before) (List.take n after) = k then
            Some before
        else
            match after with
            | [] -> None
            | x :: after -> go (x :: before) after

    go [] pattern |> Option.map (List.length >> int64)

let patternScore k pattern =
    let rowReflect = findReflectionWithDiffCount k pattern |> Option.defaultValue 0L

    let columnReflect =
        findReflectionWithDiffCount k (List.transpose pattern) |> Option.defaultValue 0L

    rowReflect * 100L + columnReflect

let part1 = patterns |> List.map (patternScore 0) |> List.sum
let part2 = patterns |> List.map (patternScore 1) |> List.sum

printfn $"{part1}"
printfn $"{part2}"
