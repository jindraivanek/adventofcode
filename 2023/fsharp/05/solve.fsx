#time
open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

module String =
    let split (sep: char) (s: string) =
        s.Split([|sep|], System.StringSplitOptions.RemoveEmptyEntries)

module List =
    let splitWhen (pred: 'a -> bool) (xs: 'a list) =
        let rec loop acc xs =
            match xs with
            | [] -> [List.rev acc]
            | x::xs ->
                if pred x then
                    List.rev acc :: loop [] xs
                else
                    loop (x::acc) xs
        loop [] xs

let lines = System.IO.File.ReadAllLines("sample") |> Array.toList

let seeds =
    match List.head lines with
    | Match "seeds: +(.*)" [ nums] -> String.split ' ' nums |> Array.map int64
    |> Array.toList
let maps =
    let mapParts = lines |> List.skip 2 |> List.splitWhen (fun line -> line = "")
    let maps =
        mapParts |> List.map (fun mapLines ->
            let source, dest =
                match List.head mapLines with
                | Match "(.*)-to-(.*) map:" [ source; dest] -> source, dest
            let map =
                List.tail mapLines |> List.collect (fun line -> 
                    let xs = String.split ' '  line |> Array.map int64
                    [xs[1], xs[0]; xs[1] + xs[2] - 1L, xs[0] + xs[2] - 1L])
                |> List.sort
            let minMap = map |> List.head |> fst
            let maxMap = map |> List.last |> fst
            let map = if minMap = 0L then map else (0L, 0L) :: map
            let map = map @ [maxMap + 1L, maxMap + 1L]
            source, (dest, map)      
            ) |> Map.ofList
    maps

let seedsToLocations seedName locationName seeds =
    let findMap x mapList =
        mapList |> List.pairwise |> List.tryFind (fun ((x1, _), (x2, _)) -> x1 <= x && x2 > x) 
        |> Option.map (fun ((x1, y),_) -> y + x - x1) |> Option.defaultValue x
    let rec go mapKey xs =
        //printfn $"{mapKey} %A{xs}"
        if mapKey = locationName then
            xs
        else
            let nextMapKey, nextMap = maps[mapKey]
            //printfn $"{nextMapKey} %A{nextMap}"
            go nextMapKey (xs |> List.map (fun x -> nextMap |> findMap x))
    go seedName seeds

let seedRangesToLocations seedName locationName seeds =
    let seedRanges = seeds |> List.chunkBySize 2 |> List.collect (fun [x;y] -> [x, x; x+y-1L, x+y-1L]) |> List.sort
    let splitRangeMap mapList ((a, ay), (b, by)) =
        let splitted =
            mapList |> List.pairwise |> List.choose (fun ((x1, y1), (x2, y2)) -> 
                if b < x1 then None 
                elif a >= x1 && b <= x2 then 
                    printfn $"split1 {a} {b} {((x1, y1), (x2, y2))} %A{[a, y1+(a-x1); b, y1+(b-x1)]}"
                    Some [a, y1+(a-x1); b, y1+(b-x1)]
                elif a >= x1 && a <= x2 && b > x2 then 
                    printfn $"split2 {a} {b} {((x1, y1), (x2, y2))} %A{[a, y1+(a-x1); x2, y2]}"
                    Some [a, y1+(a-x1); x2, y2]
                elif a < x1 && b <= x2 then 
                    printfn $"split3 {a} {b} {((x1, y1), (x2, y2))} %A{[x1, y1; b, y1+(b-x1)]}"
                    Some [a, ay; b, y1+(b-x1)] 
                else None)
            |> List.concat
        if splitted = [] then
            [a, ay; b, by]
        else
            printfn $"splitted {a} {b} %A{splitted}"
            splitted

    let rangesMap ranges mapList = ranges |> List.pairwise |> List.collect (splitRangeMap mapList) |> List.sort |> List.distinct
    
    let rec go mapKey xs =
        printfn $"{mapKey} -> %A{xs}"
        if mapKey = locationName then
            xs
        else
            let nextMapKey, nextMap = maps[mapKey]
            printfn $"map {nextMapKey} %A{nextMap}"
            go nextMapKey (rangesMap xs nextMap)
    go seedName seedRanges

let part1 = seedsToLocations "seed" "location" seeds |> Seq.min

let part2 = seedRangesToLocations "seed" "location" seeds |> Seq.min

printfn $"{part1}"
printfn $"{part2}"
