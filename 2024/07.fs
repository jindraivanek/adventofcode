module day07
open Common

let parseInput (lines: string[]) =
    lines |> Seq.map (fun s -> s.Split([|' '; ':'|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int64 |> fun xs -> xs[0], Seq.tail xs |> Seq.toList)

let rec checkOperators x xs =
    match xs with
    | [a] -> a = x
    | a::xs when x % a = 0L -> checkOperators (x/a) xs || checkOperators (x-a) xs
    | a::xs  -> checkOperators (x-a) xs
    | _ -> false

let part1 lines = 
   parseInput lines
   |> Seq.filter (fun (x, xs) -> checkOperators x (List.rev xs))
   |> Seq.map fst

// let checkOperators2 target xs =
//     let rec loop target acc x xs =
//         printfn "%A %A %A %A" target acc x xs
//         let mulWithAcc x = if acc = 0L then x else acc * x
//         let core () =
//             match xs with
//             | [a] -> 
//                 printfn "%A" (a = x)
//                 a = x
//             | a::xs when x % a = 0L -> loop target (mulWithAcc a) (x/a) xs || loop target (acc+a) (x-a) xs
//             | a::xs  -> loop target (acc+a) (x-a) xs
//             | _ -> false
//         match targetSplits |> List.tryFind (fun (s1, s2) -> acc = s2 && acc > 0L) with
//         | Some(s1, s2) -> 
//             printfn "%A || %A" s1 s2
//             loop s1 0L s1 xs || core ()
//         | _ -> core ()
//     printfn "--> %A = %A" target xs
//     loop target 0 target xs

let checkOperators2 x xs =
    let bruteforceCheck target xs =
        let rec loop acc x xs =
            let mulWithAcc a = if x = 0L then a else a * x
            match xs with
            | [] when x = target ->
                failwithf "BRUTE FAIL: %s = %A" acc target
            | a::xs -> loop $"{acc} + {a}" (x+a) xs || loop $"{acc} * {a}" (mulWithAcc a) xs || loop $"{acc} || {a}" ((string x + string a) |> int64) xs
            | _ -> false
        loop "" 0L (List.rev xs)
    let rec loop x xs =
        //printfn "%A %A" x xs
        let numSplit a =
            let s = string x
            [s.Length-1..-1..1] |> List.map (fun i -> int64 s[0..i-1], int64 s[i..])
            |> List.tryFind (fun (s1, s2) -> s2 = a)
        match xs with
        | _ when x < 0L -> false
        | [a] when a = x -> 
            //printfn "TRUE"
            true
        | a::xs ->
            match numSplit a with
            | Some(s1, _) when x % a = 0L -> loop s1 xs || loop (x/a) xs || loop (x-a) xs
            | Some(s1, _) -> loop s1 xs || loop (x-a) xs
            | _ when x % a = 0L -> loop (x/a) xs || loop (x-a) xs
            | _  -> loop (x-a) xs
        | _ -> 
            //printfn "FALSE"
            false
    //printfn "--> %A %A" x xs
    let r = loop x xs
    // if not r && bruteforceCheck x xs then
    //     printfn "BRUTE TRUE"
    r

let part2 lines = 
    parseInput lines
    |> Seq.filter (fun (x, xs) -> checkOperators2 x (List.rev xs))
    |> Seq.map fst

let sol = {
    Day = 7
    Part1 = solution part1 (Seq.sum >> string)
    Part2 = solution part2 (Seq.sum >> string) // 93546203805610 too low
}
