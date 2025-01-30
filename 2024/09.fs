module day09

open Common

let parseInput (lines: string[]) =
    let allBlocks = lines |> Seq.head |> Seq.map (string >> int) |> Seq.indexed

    let blocks =
        allBlocks
        |> Seq.filter (fun (i, _) -> i % 2 = 0)
        |> Seq.map (fun (i, x) -> i / 2, x)
        |> Map.ofSeq

    blocks,
    allBlocks
    |> Seq.collect (fun (i, x) -> Seq.replicate x (if i % 2 = 1 then None else (Some(i / 2))))
    |> Seq.toList

let moveBlocks wholeBlock (blocks: Map<int, int>) xs =
    let swap i j k (arr: _[]) =
        for l in 0 .. k - 1 do
            let tmp = arr[i + l]
            arr[i + l] <- arr[j + l]
            arr[j + l] <- tmp

    let freeSpaces (arr: _[]) =
        seq {
            let mutable i = 0

            while i < arr.Length do
                let x =
                    Seq.initInfinite (fun j -> arr[i + j]) |> Seq.takeWhile ((=) None) |> Seq.length

                if x > 0 then
                    i, x

                i <- i + (max x 1)
        }
        |> Seq.toArray

    let rs =
        if wholeBlock then
            blocks
            |> Map.toSeq
            |> Seq.sortDescending
            |> Seq.map (fun (x, l) -> List.findIndex ((=) (Some x)) xs, l, x)
            |> Seq.toList
        else
            xs
            |> List.indexed
            |> List.rev
            |> List.choose (fun (i, x) -> x |> Option.map (fun x -> i, x))
            |> List.map (fun (i, x) -> i, 1, x)

    let arr = xs |> List.toArray
    let mutable freeSpacesMut = freeSpaces arr

    let findLeftMostFreeSpace k =
        freeSpacesMut |> Seq.tryFind (fun (_, x) -> x >= k)

    let rec loop rs =
        rs
        |> List.iter (fun (j, k, _) ->
            match findLeftMostFreeSpace k with
            | Some(i, x) when i < j ->
                swap i j k arr
                let index = freeSpacesMut |> Seq.findIndex (fun (l, _) -> l = i)
                freeSpacesMut[index] <- (i + k, x - k)
            | _ -> ())

        arr |> List.ofArray

    loop rs

let part1 lines =
    let xs = parseInput lines ||> moveBlocks false

    xs
    |> Seq.mapi (fun i x -> x |> Option.defaultValue 0 |> (*) i |> int64)
    |> Seq.sum

let part2 lines =
    let xs = parseInput lines ||> moveBlocks true

    xs
    |> Seq.mapi (fun i x -> x |> Option.defaultValue 0 |> (*) i |> int64)
    |> Seq.sum

let sol =
    { Day = 9
      Part1 = solution part1 string
      Part2 = solution part2 string }
