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

let step _ (state: State) =
    let gatesToCompute = state.Gates |> List.filter (fun (a, b, c, _) -> Map.containsKey a state.Wires && Map.containsKey b state.Wires && not(Map.containsKey c state.Wires))
    if List.isEmpty gatesToCompute then None
    else
        let wires =
            gatesToCompute |> List.fold (fun w (a, b, c, f) ->
                let x = f state.Wires[a] state.Wires[b]
                //printfn "%A <- %A" c x
                w |> Map.add c x) state.Wires
        Some { Wires = wires; Gates = state.Gates }
        
let numberFromBits bits =
    bits |> Seq.map (fun (s: string, b) -> int64 b <<< (int s[1..])) |> Seq.sum

let wiresWithPrefix (prefix: string) wires = wires |> Map.toSeq |> Seq.filter (fun (k: string, _) -> k.StartsWith(prefix)) |> Seq.sort

let numberFromWires (prefix: string) wires =
    wires |> wiresWithPrefix prefix |> numberFromBits

let expectedResult state =
    let x = numberFromWires "x" state.Wires
    let y = numberFromWires "y" state.Wires
    x + y

let intToBitList n =
    let rec loop acc k n =
        if k = 0 then acc
        else loop ((n &&& 1L) :: acc) (k-1) (n >>> 1)
    loop [] 46 n |> List.rev

let intToBitString n = intToBitList n |> List.rev |> List.map string |> String.concat ""

let part1Sol = {
    Init = parseInput
    Step = step
    Result = (fun s -> numberFromWires "z" s.Wires |> intToBitString)
}

let dependsOn (state: State) wire =
    let rec loop wire =
        let gates = state.Gates |> List.filter (fun (_, _, c, _) -> c = wire)
        let wires = gates |> List.collect (fun (a, b, _, _) -> [a; b])
        wires |> List.collect loop |> List.append wires
    loop wire |> List.distinct

let rec depending (state: State) wire =
    let gates = state.Gates |> List.filter (fun (a, b, _, _) -> a = wire || b = wire)
    let wires = gates |> List.map (fun (_, _, c, _) -> c)
    wires |> List.collect (depending state) |> List.append wires

let part2 lines = 
    let s = parseInput lines
    let resState = run s step |> Seq.last
    let run s = run s step |> Seq.last |> _.Wires |> numberFromWires "z"
    let e = expectedResult s
    printfn "%s" (run s |> intToBitString)
    printfn "%s" (intToBitString e)
    let zWires = wiresWithPrefix "z"resState.Wires |> Seq.map fst |> Seq.toList
    let expectedWires = e |> intToBitList |> List.zip zWires
    let resultWires = run s |> intToBitList |> List.zip zWires
    let diffWires = List.zip expectedWires resultWires |> List.filter (fun (a, b) -> a <> b) |> List.map (fst >> fst)
    let dependsWires = diffWires |> List.collect (dependsOn s) |> List.filter (fun s -> not(s.StartsWith "x" || s.StartsWith "y")) |> List.distinct
    let affectWiresMap = dependsWires |> List.map (fun w -> w, Set.intersect (set diffWires) (depending s w |> set)) |> Map.ofList
    printfn "%A" expectedWires
    printfn "%A" resultWires
    printfn "%A" diffWires
    printfn "%A" dependsWires
    printfn "%A" affectWiresMap
    let outWires = s.Gates |> List.map (fun (_, _, c, _) -> c) |> List.distinct
    let swapCandicates = Set.intersect (dependsWires |> set) (outWires |> set) |> List.ofSeq
    let swapCandicates = affectWiresMap |> Map.filter (fun k v -> v |> Seq.length >= 2) |> Map.toSeq |> Seq.sortByDescending (snd >> Seq.length) |> Seq.map fst |> List.ofSeq
    printfn "%A" swapCandicates
    let pairs = Seq.allPairs swapCandicates swapCandicates |> Seq.filter (fun (a, b) -> a < b)
    let swapPairs4 = 
        Seq.allPairs (Seq.allPairs pairs pairs) (Seq.allPairs pairs pairs) 
        |> Seq.filter (fun (((a, b), (c, d)),((e, f), (g, h))) -> 
            a < c && c < e && e < g && 
            a <> c && a <> d && a <> e && a <> f && a <> g && a <> h &&
            b <> c && b <> d && b <> e && b <> f && b <> g && b <> h &&
            c <> e && c <> f && c <> g && c <> h &&
            d <> e && d <> f && d <> g && d <> h &&
            e <> g && e <> h &&
            f <> g && f <> h
            ) 
        |> Seq.map (fun ((a, b), (c, d)) -> set [ a; b; c; d ]) 
        //|> Seq.filter (fun s -> s |> Seq.collect (fun (a,b) -> [a;b]) |> set |> Set.count = 8)    
        //|> Seq.filter (fun s -> s |> Seq.length = 4)    
        |> Seq.distinct
    swapPairs4 |> Seq.mapi (fun i pairs ->
        let outMapping = pairs |> Seq.collect (fun (a, b) -> [a, b; b, a]) |> Map.ofSeq
        let newState = { s with Gates = s.Gates |> List.map (fun (a, b, c, op) -> let c = outMapping |> Map.tryFind c |> Option.defaultValue c in (a, b, c, op)) }
        let r = run newState
        if i % 1000 = 0 then printfn "%A -> %A" pairs r
        r, pairs
    ) |> Seq.find (fun (x, _) -> x = e)

let sol = {
    Day = 24
    Part1 = part1Sol
    Part2 = solution part2 (string)
}
