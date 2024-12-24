module day24
open Common

type State = { Wires: Map<string, int>; Gates: list<string * string * string * (int -> int -> int)> }

let parseInput (lines: string[]) =
    let [wiresLines; gatesLines] = splitBy ((=) "") lines |> Seq.toList
    let wires = wiresLines |> List.map (fun s -> s.Split(": ") |> fun xs -> xs[0], int xs[1]) |> Map.ofList
    let op = function
        | "OR" -> (|||)
        | "AND" -> (&&&)
        | "XOR" -> (^^^)
    let gates = 
        gatesLines |> List.map (fun s -> s.Split(" -> ") |> fun xs -> 
            let gs = xs[0].Split(" ")
            gs[0], gs[2], xs[1], op gs[1])
    { Wires = wires; Gates = gates }

let step (state: State) =
    let gatesToCompute = state.Gates |> List.filter (fun (a, b, c, _) -> Map.containsKey a state.Wires && Map.containsKey b state.Wires && not(Map.containsKey c state.Wires))
    if List.isEmpty gatesToCompute then None
    else
        let wires =
            gatesToCompute |> List.fold (fun w (a, b, c, f) ->
                let x = f state.Wires[a] state.Wires[b]
                printfn "%A <- %A" c x
                w |> Map.add c x) state.Wires
        Some { Wires = wires; Gates = state.Gates }
        
let computeResult bits =
    bits |> Seq.map (fun (s: string, b) -> int64 b <<< (int s[1..])) |> Seq.sum
let part1Sol = {
    Init = parseInput
    Step = fun _ -> step
    Result = (fun s -> s.Wires |> Map.toSeq |> Seq.filter (fun (k, _) -> k.StartsWith("z")) |> Seq.sort |> Seq.toList |> computeResult |> string)
}
let part2 lines = 
    [0]

let sol = {
    Day = 24
    Part1 = part1Sol
    Part2 = solution part2 (Seq.length >> string)
}
