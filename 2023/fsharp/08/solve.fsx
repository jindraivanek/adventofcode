#time
open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let lines = System.IO.File.ReadAllLines("__SOURCE_DIRECTORY__/../../../../input/2023/08.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let instructions = lines[0]

let paths =
    lines
    |> Seq.skip 2
    |> Seq.collect (function
        | Match "(.*) = \((.*), (.*)\)" [ n; l; r ] -> [ (n, 'L'), l; (n, 'R'), r ])
    |> Map.ofSeq

let instrCycle = Seq.initInfinite (fun i -> instructions.[i % instructions.Length])

let makeSteps xs startNode =
    (startNode, xs) ||> Seq.scan (fun n dir -> paths.[n, dir])


let findEndCycles xs startNodes endCondition =
    startNodes
    |> List.map (fun n ->
        let ends =
            makeSteps xs n
            |> Seq.indexed
            |> Seq.filter (snd >> endCondition)
            |> Seq.map fst
            |> Seq.take 2
            |> Seq.toList

        let offset = ends.[0]
        let cycleLength = ends.[1] - offset
        assert (offset = cycleLength)
        cycleLength)

let part1 =
    makeSteps instrCycle "AAA" |> Seq.takeWhile (((=) "ZZZ") >> not) |> Seq.length

printfn $"{part1}"

let startNodes =
    paths
    |> Map.keys
    |> Seq.map fst
    |> Seq.filter (fun s -> s.EndsWith("A"))
    |> Seq.distinct
    |> Seq.toList

let endIndexes =
    findEndCycles instrCycle startNodes (fun n -> n.EndsWith "Z") |> List.map int64

let firstStep = List.min endIndexes

let gcd a b =
    let rec gcd' a b = if b = 0L then a else gcd' b (a % b)
    gcd' (abs a) (abs b)

let part2 = endIndexes |> List.fold (fun acc i -> (acc * i) / gcd acc i) firstStep
printfn $"{part2}"
