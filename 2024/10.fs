module day10
open Common

let dirs = [ (0, -1); (-1, 0); (0, 1); (1, 0) ]
let posPlus (a, b) (c, d) = (a + c, b + d)
let init (lines: string[]) =
    let m = lines |> Seq.mapi (fun j l -> l |> Seq.mapi (fun i c -> (i, j), int (string c))) |> Seq.collect id |> Map.ofSeq
    let starts = m |> Map.filter (fun _ v -> v = 0) |> Map.toSeq |> Seq.map fst |> Seq.map (fun p -> p, 0) |> List.ofSeq
    let neighF p = dirs |> List.map (fun d -> posPlus p d) |> List.filter (fun v -> Map.containsKey v m && m[p] = m[v] - 1) |> List.map (fun n -> n, 1)
    let isFinish p = m[p] = 9
    {| Starts = starts; NeighF = neighF; IsFinish = isFinish |}

let dijkstra (initNodes: ('n * int) list) (neighF: 'n -> ('n * int) list) (finishCond: 'n -> bool) =
    let pq = System.Collections.Generic.PriorityQueue<'n, int>()
    let visited = System.Collections.Generic.HashSet<_>()
    let score = System.Collections.Generic.Dictionary<_,_ Set>()

    let dequeue () =
        let (success, n, p) = pq.TryDequeue()

        if success then
            //if visited.Count % 1000 = 0 then printfn "%A" (node, p)
            Some(n, p)
        else
            None

    initNodes |> Seq.iter (fun (n, p) -> pq.Enqueue(n, p); score.Add(n, set [n]))

    let rec step () =
        match dequeue () with
        | Some((node), p) when visited.Contains(node) ->
            step ()
        //| Some((node), p) when finishCond node -> step ()
        | None -> score |> Seq.filter (fun kvp -> finishCond kvp.Key) |> Seq.sumBy (fun kvp -> printfn "%A %A" kvp.Key kvp.Value; kvp.Value.Count)
        | Some((node), p) ->
            visited.Add(node) |> ignore

            neighF node
            |> Seq.iter (fun (n, p') ->
                //if not(visited.Contains(n)) then
                    if score.ContainsKey n then score[n] <- Set.union score[n] score[node] else score.Add(n, score[node])
                    pq.Enqueue(n, p + p')

                    if finishCond n then
                        printfn $"%A{(n, score[n])}")

            step ()

    step ()


let part1 lines = 
    let s = init lines
    let r = dijkstra s.Starts s.NeighF s.IsFinish
    r

let part2 lines = 
    let s = init lines
    s.Starts

let sol = {
    Day = 10
    Part1 = solution part1 (string)
    Part2 = solution part2 (Seq.length >> string)
}
