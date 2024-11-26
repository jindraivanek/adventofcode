#time

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../../input/2023/25.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let edges =
    lines
    |> Seq.collect (fun l ->
        let [| w; xs |] = l.Split(':')

        xs.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s -> w, s))
    |> Seq.toList

let nodeMap =
    edges
    |> Seq.collect (fun (v: string, w: string) -> [ v; w ])
    |> Seq.distinct
    |> Seq.mapi (fun i x -> x, i)
    |> Map.ofSeq

let nodeMapRev = nodeMap |> Map.toSeq |> Seq.map (fun (k, v) -> v, k) |> Map.ofSeq

let edgeByNodes =
    edges
    |> Seq.collect (fun (v, w) -> [ v, w; w, v ])
    |> Seq.groupBy fst
    |> Seq.map (fun (v, ws) -> v, ws |> Seq.map snd |> Seq.toList)
    |> Map.ofSeq

let mincut edges s t =
    let V = Map.count nodeMap

    let bfs (rGraph: int[,]) s t (parent: _[]) =
        let visited = Array.init V (fun _ -> false)
        let q = System.Collections.Generic.Queue<int>()
        q.Enqueue(s)
        visited.[s] <- true
        parent.[s] <- -1

        while q.Count <> 0 do
            let u = q.Dequeue()

            for v in 0 .. V - 1 do
                if not visited.[v] && rGraph.[u, v] > 0 then
                    q.Enqueue(v)
                    parent.[v] <- u
                    visited.[v] <- true

        visited.[t]

    let rec dfs (rGraph: int[,]) s (visited: _[]) =
        visited.[s] <- true

        for i in 0 .. V - 1 do
            if rGraph.[s, i] > 0 && not visited.[i] then
                dfs rGraph i visited

    let minCut (graph: int[,]) s t =
        let rGraph = Array2D.init V V (fun u v -> graph.[u, v])
        let parent = Array.zeroCreate V

        while bfs rGraph s t parent do
            let mutable path_flow = System.Int32.MaxValue
            let mutable v = t

            while v <> s do
                let u = parent.[v]
                path_flow <- min path_flow rGraph.[u, v]
                v <- parent.[v]

            v <- t

            while v <> s do
                let u = parent.[v]
                rGraph.[u, v] <- rGraph.[u, v] - path_flow
                rGraph.[v, u] <- rGraph.[v, u] + path_flow
                v <- parent.[v]

        let visited = Array.init V (fun _ -> false)
        dfs rGraph s visited

        let cut =
            [ for i in 0 .. V - 1 do
                  for j in 0 .. V - 1 do
                      if visited.[i] && not visited.[j] && graph.[i, j] > 0 then
                          yield nodeMapRev[i], nodeMapRev[j] ]

        let partition =
            visited
            |> Seq.mapi (fun i x -> i, x)
            |> Seq.filter snd
            |> Seq.map fst
            |> Seq.map (fun x -> nodeMapRev[x])
            |> Seq.toList

        cut, partition

    let g = Array2D.zeroCreate V V

    edges
    |> Seq.iter (fun (v, w) ->
        g.[nodeMap[v], nodeMap[w]] <- 1
        g.[nodeMap[w], nodeMap[v]] <- 1)

    minCut g s t

let minCutLoop edges =
    seq {
        for i in 0 .. Map.count nodeMap - 1 do
            for j in i + 1 .. Map.count nodeMap - 1 do
                let cut, part = mincut edges i j
                let c = Seq.length cut
                printfn $"{i} {j} {c}"

                if c = 3 then
                    yield cut, part
    }
    |> Seq.head

printfn "%A" edgeByNodes
mincut edges |> printfn "%A"

let part1 =
    let _, part = minCutLoop edges
    part |> Seq.length |> (fun x -> x * (nodeMap.Count - x))

let part2 = 0

printfn $"{part1}"
printfn $"{part2}"
