module day19

open Common

let parseInput (lines: string[]) =
    let ts = lines[0].Split(", ") |> Seq.toList
    let xs = lines[2..] |> Seq.toList
    ts, xs

type Trie = Trie of Map<char, bool * Trie>

let mkTrie (ts: string list) =
    let rec build (ts: string list) =
        ts
        |> List.filter ((<>) "")
        |> List.groupBy (fun t -> t[0])
        |> List.map (fun (c, ts) ->
            let ts2 = ts |> List.map (fun t -> t.Substring(1))
            let isMark = ts2 |> Seq.exists ((=) "")
            c, (isMark, (build ts2 |> Map.ofList |> Trie)))

    build ts |> Map.ofList |> Trie

let getStarts (Trie t) s =
    let rec go acc x s t =
        match s, t with
        | "", _ -> acc
        | s, Trie t ->
            let c = s[0]
            let s = s[1..]

            match Map.tryFind c t with
            | None -> acc
            | Some(false, t2) -> go acc (c :: x) s t2
            | Some(true, t2) -> go ((List.rev >> List.map string >> String.concat "") (c :: x) :: acc) (c :: x) s t2

    go [] [] s (Trie t)

let combNumber =
    memoizeRec
    <| fun recF (t, s) ->
        let xs = getStarts t s

        xs
        |> Seq.sumBy (fun x -> if x.Length = s.Length then 1L else recF (t, s[x.Length ..]))

let part1 lines =
    let ts, xs = parseInput lines
    let t = mkTrie ts
    xs |> List.filter (fun x -> combNumber (t, x) > 0L)

let part2 lines =
    let ts, xs = parseInput lines
    let t = mkTrie ts
    xs |> List.map (fun x -> combNumber (t, x))

let sol =
    { Day = 19
      Part1 = solution part1 (Seq.length >> string)
      Part2 = solution part2 (Seq.sum >> string) }
