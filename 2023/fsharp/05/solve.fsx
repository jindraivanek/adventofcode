#time
open System.Text.RegularExpressions

let teePrint label x =
    printfn $"{label}: {x}"
    x

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

module String =
    let split (sep: char) (s: string) =
        s.Split([| sep |], System.StringSplitOptions.RemoveEmptyEntries)

module List =
    let splitWhen (pred: 'a -> bool) (xs: 'a list) =
        let rec loop acc xs =
            match xs with
            | [] -> [ List.rev acc ]
            | x :: xs ->
                if pred x then
                    List.rev acc :: loop [] xs
                else
                    loop (x :: acc) xs

        loop [] xs

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/05.txt") |> Array.toList

let seeds =
    match List.head lines with
    | Match "seeds: +(.*)" [ nums ] -> String.split ' ' nums |> Array.map int64
    |> Array.toList

let maps =
    let mapParts = lines |> List.skip 2 |> List.splitWhen (fun line -> line = "")

    let maps =
        mapParts
        |> List.map (fun mapLines ->
            let source, dest =
                match List.head mapLines with
                | Match "(.*)-to-(.*) map:" [ source; dest ] -> source, dest

            let map =
                List.tail mapLines
                |> List.collect (fun line ->
                    let xs = String.split ' ' line |> Array.map int64
                    [ xs[1], xs[0]; xs[1] + xs[2] - 1L, xs[0] + xs[2] - 1L ])
                |> List.sort

            let minMap = map |> List.head |> fst
            let maxMap = map |> List.last |> fst
            let map = if minMap = 0L then map else (0L, 0L) :: map
            let map = map @ [ maxMap + 1L, maxMap + 1L ]
            source, (dest, map))
        |> Map.ofList

    maps

let reverseMaps =
    let mapParts = lines |> List.skip 2 |> List.splitWhen (fun line -> line = "")

    let maps =
        mapParts
        |> List.map (fun mapLines ->
            let source, dest =
                match List.head mapLines with
                | Match "(.*)-to-(.*) map:" [ source; dest ] -> source, dest

            let map =
                List.tail mapLines
                |> List.collect (fun line ->
                    let xs = String.split ' ' line |> Array.map int64
                    [ xs[0], xs[0] + xs[2] - 1L, xs[1] - xs[0] ])
                |> List.sort

            dest, (source, map))
        |> Map.ofList

    maps

let seedsToLocations seedName locationName seeds =
    let findMap x mapList =
        mapList
        |> List.pairwise
        |> List.tryFind (fun ((x1, _), (x2, _)) -> x1 <= x && x2 > x)
        |> Option.map (fun ((x1, y), _) -> y + x - x1)
        |> Option.defaultValue x

    let rec go mapKey xs =
        //printfn $"{mapKey} %A{xs}"
        if mapKey = locationName then
            xs
        else
            let nextMapKey, nextMap = maps[mapKey]
            //printfn $"{nextMapKey} %A{nextMap}"
            go nextMapKey (xs |> List.map (fun x -> nextMap |> findMap x))

    go seedName seeds

let seedMap =
    seeds
    |> List.chunkBySize 2
    |> List.collect (fun [ x; y ] -> [ (x, x + y - 1L, 0L) ])
    |> List.sort

let backFind seedName locationName x =
    let findMap x mapList =
        mapList
        |> List.tryFind (fun (a, b, _) -> a <= x && x <= b)
        |> Option.map (fun (_, _, d) -> x + d)

    let rec go mapKey x =
        //printfn $"{mapKey} %A{xs}"
        if mapKey = seedName then
            //printfn "seed %A" x
            findMap x seedMap |> Option.isSome
        else
            let nextMapKey, nextMap = reverseMaps[mapKey]
            //printfn $"{nextMapKey} %A{nextMap}"
            go nextMapKey (nextMap |> findMap x |> Option.defaultValue x)

    go locationName x

let findMinLocation seedName locationName =
    Seq.initInfinite (fun x -> int64 x)
    |> Seq.filter (backFind seedName locationName)
    |> Seq.head

let part1 = seedsToLocations "seed" "location" seeds |> Seq.min

let part2 = findMinLocation "seed" "location"

printfn $"{part1}"
printfn $"{part2}"
