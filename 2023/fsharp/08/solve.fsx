(*
--- Day 8: Haunted Wasteland ---
You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching. When you turn to warn the Elf, she disappears before your eyes! To be fair, she had just finished warning you about ghosts a few minutes ago.

One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle input) about how to navigate the desert. At least, you're pretty sure that's what they are; one of the documents contains a list of left/right instructions, and the rest of the documents seem to describe some kind of network of labeled nodes.

...

This format defines each node of the network individually. For example:

RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)

...

Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?

--- Part Two ---

LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)

Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?

> Part1 solution doesn't scale, we need to detect cycles of beign on end node, and then use them to calculate the answer.
*)

#time
open System.Text.RegularExpressions

let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let lines = System.IO.File.ReadAllLines("input")
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

let part1 =
    makeSteps instrCycle "AAA" |> Seq.takeWhile (((=) "ZZZ") >> not) |> Seq.length

printfn $"{part1}"

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

let part2 = 
    // minimum common multiple
    endIndexes |> List.fold (fun acc i -> (acc * i) / gcd acc i) firstStep
printfn $"{part2}"
