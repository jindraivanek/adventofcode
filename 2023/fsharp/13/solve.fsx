(*
--- Day 13: Point of Incidence ---

...

To find the reflection in each pattern, you need to find a perfect reflection across either a horizontal line between two rows or across a vertical line between two columns.

In the first pattern, the reflection is across a vertical line between two columns; arrows on each of the two columns point at the line between the columns:

123456789
    ><   
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.
    ><   
123456789

To summarize your pattern notes, add up the number of columns to the left of each vertical line of reflection; to that, also add 100 multiplied by the number of rows above each horizontal line of reflection.

Find the line of reflection in each of the patterns in your notes. What number do you get after summarizing all of your notes?

--- Part Two ---
Upon closer inspection, you discover that every mirror has exactly one smudge: exactly one . or # should be the opposite type.

> (smudge = exactly one error in reflection)

In each pattern, fix the smudge and find the different line of reflection. What number do you get after summarizing the new reflection line in each pattern in your notes?
*)

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

let patterns =
    lines |> Seq.toList |> chunkBy ((<>) "") |> List.map (List.map Seq.toList)

let diffCount xs ys =
    List.zip xs ys |> List.filter (fun (x, y) -> x <> y) |> List.length

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
