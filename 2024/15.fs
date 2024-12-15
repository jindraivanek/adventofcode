module day15
open Common
open Common.Grid

type State = {
    Floor: Set<int * int>
    Walls: Set<int * int>
    Boxes: Set<int * int>
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
        |> Seq.map (Seq.collect (fun c -> if c = '@' && wide = 2 then seq ['@'; '.'] else Seq.replicate wide c))
        |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> c, (i, j))) |> Seq.collect id
    let w = grid |> Seq.filter (fun (c, _) -> c = '#') |> Seq.map snd |> set
    let b = grid |> Seq.filter (fun (c, _) -> c = 'O') |> Seq.map snd |> set
    let f = grid |> Seq.filter (fun (c, _) -> c = '.') |> Seq.map snd |> set
    let r = grid |> Seq.find (fun (c, _) -> c = '@') |> snd
    let i = instr |> Seq.map (fun c -> instrMap[c]) |> List.ofSeq
    { Floor = f; Walls = w; Boxes = b; Robot = r; Instructions = i; Wide = wide }
    
let rec moveBox isFirst p d state =
    let boxCells b = [0..state.Wide-1] |> List.map (fun i -> posPlus b (posMult (1,0) i)) |> set
    let t = posPlus p d
    let s = { state with Boxes = state.Boxes |> (fun bs -> if isFirst then bs - boxCells p else bs) |> Set.union (boxCells t) }
    if Set.contains t state.Walls then None
    elif Set.contains t state.Boxes then moveBox false t d s 
    else Some s

let step _ state =
    //printfn "%A" (Seq.tryHead state.Instructions, state.Robot, state.Boxes)
    let r = state.Robot
    match state.Instructions with
    | [] -> None
    | d :: ds -> 
        let state = { state with Instructions = ds } 
        let t = posPlus r d
        let s = { state with Robot = t }
        (if Set.contains t s.Boxes then moveBox true t d s
         elif Set.contains t s.Walls then None
         else Some s
        )|> Option.defaultValue state |> Some

let part1 =
    { Init = parseInput 1
      Step = step
      Result = (fun s -> s.Boxes |> Seq.sumBy (fun (x, y) -> 100 * y + x) |> string) }

let part2 = 
    { Init = parseInput 2
      Step = step
      Result = (fun s -> s.Boxes |> Seq.sortBy (fun (x, y) -> y, x) |> Seq.chunkBySize 2 |> Seq.map Seq.head |> Seq.filter (fun (x, _) -> x % 2 = 0) |> Seq.sumBy (fun (x, y) -> 100 * y + x) |> string) }


let sol = {
    Day = 15
    Part1 = part1
    Part2 = part2 //3875951 too high
}
