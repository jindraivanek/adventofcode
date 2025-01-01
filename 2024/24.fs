module day24

open System.Data
open Common

type State =
    { Wires: Map<string, int>
      Gates: Set<string * string * string * string>
      GateOp: string -> int -> int -> int
      Outputs: Map<string, string list>
      Inputs: Map<string, string list> }

let recomputeInputsOutputs (state: State) =
    let gateOutputMap =
        state.Gates
        |> Set.toList
        |> List.collect (fun (a, b, op, c) -> [ a, c; b, c ])
        |> List.groupBy fst
        |> List.map (fun (a, xs) -> a, List.map snd xs)
        |> Map.ofList

    let geteInputsMap =
        state.Gates
        |> Set.toList
        |> List.map (fun (a, b, op, c) -> c, [ a; b ])
        |> Map.ofList

    { state with
        Inputs = geteInputsMap
        Outputs = gateOutputMap }

let parseInput (lines: string[]) =
    let [ wiresLines; gatesLines ] = splitBy ((=) "") lines |> Seq.toList

    let wires =
        wiresLines
        |> List.map (fun s -> s.Split(": ") |> fun xs -> xs[0], int xs[1])
        |> Map.ofList

    let op =
        function
        | "OR" -> (|||)
        | "AND" -> (&&&)
        | "XOR" -> (^^^)

    let gates =
        gatesLines
        |> List.map (fun s ->
            s.Split(" -> ")
            |> fun xs ->
                let gs = xs[0].Split(" ")
                gs[0], gs[2], xs[1], gs[1])

    let gateSet =
        gates |> List.map (fun (a, b, c, op) -> min a b, max a b, op, c) |> set

    recomputeInputsOutputs
        { Wires = wires
          Gates = gateSet
          GateOp = op
          Inputs = Map.empty
          Outputs = Map.empty }

let step _ (state: State) =
    let gatesToCompute =
        state.Gates
        |> Set.filter (fun (a, b, _, c) ->
            Map.containsKey a state.Wires
            && Map.containsKey b state.Wires
            && not (Map.containsKey c state.Wires))

    if Set.isEmpty gatesToCompute then
        None
    else
        let wires =
            gatesToCompute
            |> Set.fold
                (fun w (a, b, op, c) ->
                    let x = state.GateOp op state.Wires[a] state.Wires[b]
                    //printfn "%A <- %A" c x
                    w |> Map.add c x)
                state.Wires

        Some { state with Wires = wires }

let numberFromBits bits =
    bits |> Seq.map (fun (s: string, b) -> int64 b <<< (int s[1..])) |> Seq.sum

let wiresWithPrefix (prefix: string) wires =
    wires
    |> Map.toSeq
    |> Seq.filter (fun (k: string, _) -> k.StartsWith(prefix))
    |> Seq.sort

let numberFromWires (prefix: string) wires =
    wires |> wiresWithPrefix prefix |> numberFromBits

let expectedResult state =
    let x = numberFromWires "x" state.Wires
    let y = numberFromWires "y" state.Wires
    x + y

let intToBitList n =
    let rec loop acc k n =
        if k = 0 then
            acc
        else
            loop ((n &&& 1L) :: acc) (k - 1) (n >>> 1)

    loop [] 46 n |> List.rev

let intToBitString n =
    intToBitList n |> List.rev |> List.map string |> String.concat ""

let part1Sol =
    { Init = parseInput
      Step = step
      Result = (fun s -> numberFromWires "z" s.Wires |> string) }

let printGraph (state: State) =
    let renameWires (state: State) =
        let inputWires =
            Seq.concat [ wiresWithPrefix "x" state.Wires; wiresWithPrefix "y" state.Wires ]
            |> Seq.map fst
            |> Seq.groupBy (fun s -> int s[1..])
            |> Seq.sortBy fst
            |> Seq.toList

        let rec loop acc j (i, ws) =
            match ws |> List.choose (fun w -> state.Outputs |> Map.tryFind w) |> List.collect id with
            | [] -> acc
            | ws ->
                let map =
                    ws
                    |> List.map (fun w ->
                        if w.StartsWith "z" then
                            w, w
                        else
                            w, sprintf "%02dz_%d_%s" i j w)

                loop (acc @ map) (j + 1) (i, ws)

        inputWires
        |> List.map (fun (i, ws) ->
            loop (ws |> Seq.toList |> List.map (fun w -> w, sprintf "%02d%c" i w[0])) 0 (i, Seq.toList ws))
        |> List.concat
        |> Map.ofList

    let nameMap = renameWires state

    let gates =
        state.Gates
        |> Seq.map (fun (a, b, op, c) -> nameMap[a], op, nameMap[b], nameMap[c])
        |> Seq.map (fun (a, op, b, c) -> min a b, op, max a b, c)
        |> Seq.sortBy (fun (a, _, _, _) -> a)
        |> Seq.toList

    gates |> Seq.iter (fun (a, op, b, c) -> printfn "%s %s %s -> %s" a op b c)
    ()


let part2 lines =
    let s = parseInput lines
    // manually solved by inspecting the graph:
    printGraph s

let sol =
    { Day = 24
      Part1 = part1Sol
      Part2 = solution part2 (string) }
