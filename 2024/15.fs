module day15
open Common
open Common.Grid

type State = {
    Floor: Set<int * int>
    Walls: Set<int * int>
    Boxes: Set<int * int> Set
    Robot: int * int
    Instructions: list<int * int>
    Wide : int
}

let instrMap = "^>v<" |> Seq.mapi (fun i c -> c, dirs[i]) |> Map.ofSeq

let parseInput wide (lines: string[]) =
    let input = splitBy (fun l -> l = "") lines |> Seq.toList
    let instr = input[1] |> String.concat ""
    let grid = 
        input[0] 
        |> Seq.map (Seq.collect (fun c -> if (c = '@' || c = 'O') && wide = 2 then seq [c; '.'] else Seq.replicate wide c))
        |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> c, (i, j))) |> Seq.collect id
    let w = grid |> Seq.filter (fun (c, _) -> c = '#') |> Seq.map snd |> set
    let b = grid |> Seq.filter (fun (c, _) -> c = 'O') |> Seq.map (snd >> fun p -> set [p; posPlus p ((wide-1, 0))]) |> set
    let f = grid |> Seq.filter (fun (c, _) -> c = '.') |> Seq.map snd |> set
    let r = grid |> Seq.find (fun (c, _) -> c = '@') |> snd
    let i = instr |> Seq.map (fun c -> instrMap[c]) |> List.ofSeq
    { Floor = f; Walls = w; Boxes = b; Robot = r; Instructions = i; Wide = wide }

let boxesMap (state: State) = state.Boxes |> Seq.collect (fun bs -> bs |> Seq.map (fun b -> b, bs)) |> Map.ofSeq

let moveBox b d state =
    let boxesMap = boxesMap state
    let rec loop acc =
        let bs = acc |> Seq.collect (Set.map (posPlus d)) |> set
        let movedBoxes = bs |> Seq.choose (fun b -> boxesMap |> Map.tryFind b) |> Seq.filter (fun x -> x <> b) |> set
        if Set.intersect bs state.Walls |> Set.isEmpty |> not then None
        elif (movedBoxes - acc) <> Set.empty then loop (acc + movedBoxes) 
        else Some acc
    let movedBoxes = loop (set [b])
    movedBoxes |> Option.map (fun movedBoxes -> { state with Boxes = state.Boxes - movedBoxes |> Set.union (movedBoxes |> Set.map (Set.map (posPlus d))) })

let step _ state =
    //printfn "%A" (Seq.tryHead state.Instructions, state.Robot, state.Boxes)
    let r = state.Robot
    match state.Instructions with
    | [] -> None
    | d :: ds -> 
        let boxesMap = boxesMap state
        let state = { state with Instructions = ds } 
        let t = posPlus r d
        let s = { state with Robot = t }
        (if Map.containsKey t boxesMap then moveBox boxesMap[t] d s
         elif Set.contains t s.Walls then Some state
         else Some s
        ) |> Option.defaultValue state |> Some

let part1 =
    { Init = parseInput 1
      Step = step
      Result = (fun s -> s.Boxes |> Seq.map Seq.head |> Seq.sumBy (fun (x, y) -> 100 * y + x) |> string) }

let part2 = 
    { Init = parseInput 2
      Step = step
      Result = (fun s -> s.Boxes |> Seq.map (Seq.sort >> Seq.head) |> Seq.sumBy (fun (x, y) -> 100 * y + x) |> string) }


let sol = {
    Day = 15
    Part1 = part1
    Part2 = part2 //3875951 too high
}
