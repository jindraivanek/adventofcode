(*
--- Day 12: Hot Springs ---
In the giant field just outside, the springs are arranged into rows. For each row, the condition records show every spring and whether it is operational (.) or damaged (#). This is the part of the condition records that is itself damaged; for some springs, it is simply unknown (?) whether the spring is operational or damaged.

This list always accounts for every damaged spring, and each number is the entire size of its contiguous group (that is, groups are always separated by at least one operational spring: #### would always be 4, never 2,2).

However, the condition records are partially damaged; some of the springs' conditions are actually unknown (?). For example:

???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1

For each row, count all of the different arrangements of operational and broken springs that meet the given criteria. What is the sum of those counts?

--- Part Two ---
To unfold the records, on each row, replace the list of spring conditions with five copies of itself (separated by ?) and replace the list of contiguous groups of damaged springs with five copies of itself (separated by ,).

The first line of the above example would become:
???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3

Unfold your condition records; what is the new sum of possible arrangement counts?
*)

#time

let lines = System.IO.File.ReadAllLines("input")
//let lines = System.IO.File.ReadAllLines("sample")

let teePrint x =
    printfn $"%A{x}"
    x

let memoize f =
    let cache = System.Collections.Generic.Dictionary<_, _>()

    fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | _ ->
            let v = f x
            cache.Add(x, v)
            v

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

// ???.### 1,1,3
// .??..??...?##. 1,1,3
// ?#?#?#?#?#?#?#? 1,3,1,6
// ????.#...#... 4,1,1
// ????.######..#####. 1,6,5
// ?###???????? 3,2,1
let combNum xs gs =
    let rec recF xs curG gs =
        match xs, curG, gs with
        // correct combination
        | [], None, []
        | [], Some 0, [] -> 1L
        // '#' part
        | '#' :: xs, Some g, gs
        | '#' :: xs, None, (g :: gs)
        | '?' :: xs, Some g, gs when g > 0 -> recF xs (Some(g - 1)) gs
        // '.' part
        | '.' :: xs, Some 0, gs
        | '.' :: xs, None, gs
        | '?' :: xs, Some 0, gs -> recF xs None gs
        // '?' as '.' at the end
        | '?' :: xs, None, [] -> recF xs None []
        // main recursion - '?' as '.' or '#'
        | '?' :: xs, None, g :: gs -> recF xs (Some(g - 1)) gs + recF xs None (g :: gs)
        // invalid combination
        | _ -> 0L

    recF xs None gs

let combNumMem xs gs =
    let rec recF =
        memoize
        <| fun (xs, curG, gs) ->
            let recF x curG g = recF (x, curG, g)

            match xs, curG, gs with
            // correct combination
            | [], None, []
            | [], Some 0, [] -> 1L
            // '#' part
            | '#' :: xs, Some g, gs
            | '#' :: xs, None, (g :: gs)
            | '?' :: xs, Some g, gs when g > 0 -> recF xs (Some(g - 1)) gs
            // '.' part
            | '.' :: xs, Some 0, gs
            | '.' :: xs, None, gs
            | '?' :: xs, Some 0, gs -> recF xs None gs
            // '?' as '.' at the end
            | '?' :: xs, None, [] -> recF xs None []
            // main recursion - '?' as '.' or '#'
            | '?' :: xs, None, g :: gs -> recF xs (Some(g - 1)) gs + recF xs None (g :: gs)
            // invalid combination
            | _ -> 0L

    recF (xs, None, gs)

let combNumMemRec xs gs =
    let rec recF =
        memoizeRec
        <| fun recF (xs, curG, gs) ->
            let recF x curG g = recF (x, curG, g)

            match xs, curG, gs with
            // correct combination
            | [], None, []
            | [], Some 0, [] -> 1L
            // '#' part
            | '#' :: xs, Some g, gs
            | '#' :: xs, None, (g :: gs)
            | '?' :: xs, Some g, gs when g > 0 -> recF xs (Some(g - 1)) gs
            // '.' part
            | '.' :: xs, Some 0, gs
            | '.' :: xs, None, gs
            | '?' :: xs, Some 0, gs -> recF xs None gs
            // '?' as '.' at the end
            | '?' :: xs, None, [] -> recF xs None []
            // main recursion - '?' as '.' or '#'
            | '?' :: xs, None, g :: gs -> recF xs (Some(g - 1)) gs + recF xs None (g :: gs)
            // invalid combination
            | _ -> 0L

    recF (xs, None, gs)


let parsed =
    lines
    |> Seq.map (fun s ->
        let [| xs1; xs2 |] = s.Split(' ')
        let counts = xs2.Split(',') |> Seq.map int |> Seq.toList
        xs1, counts)
    |> Seq.toList

let unfold (xs: string, cs) =
    let xs = Seq.replicate 5 xs |> String.concat "?"
    let cs = cs |> List.replicate 5 |> List.concat
    xs, cs

let part1 () =
    parsed |> Seq.map (fun (xs, cs) -> combNum (Seq.toList xs) cs) |> Seq.sum

printfn $"{part1 ()}" // 7191

let part2 () =
    parsed
    |> Seq.map unfold
    |> Seq.map (fun (xs, cs) -> combNumMemRec (Seq.toList xs) cs |> int64)
    |> Seq.sum

printfn $"{part2 ()}" // 6512849198636
