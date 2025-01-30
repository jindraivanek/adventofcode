module day17

open Common

type Computer =
    { mutable A: int64
      mutable B: int64
      mutable C: int64
      mutable IP: int
      Program: byte[]
      mutable Output: byte ResizeArray
      mutable OutputChecksum: int64 }

let parseInput (lines: string[]) =
    let parseNumber =
        function
        | Match ".* ([0-9]+)" [ n ] -> int64 n
        | _ -> failwith "parse error"

    let parseProgram =
        function
        | Match "Program: (.*)" [ p ] -> p.Split(",") |> Seq.map byte |> Seq.toArray
        | _ -> failwith "parse error"

    { A = parseNumber lines[0]
      B = parseNumber lines[1]
      C = parseNumber lines[2]
      IP = 0
      Program = parseProgram lines[4]
      Output = ResizeArray()
      OutputChecksum = 0L }

let checksum (xs: byte seq) =
    xs |> Seq.indexed |> Seq.fold (fun c (i, x) -> c ^^^ (int64 x <<< (i % 64))) 0L

let instr opCode op c =
    let literal = int64 op

    let combo =
        match op with
        | 4uy -> c.A
        | 5uy -> c.B
        | 6uy -> c.C
        | x -> int64 x

    c.IP <- c.IP + 2

    match opCode with
    | 0uy -> c.A <- c.A / (1L <<< int32 combo)
    | 1uy -> c.B <- c.B ^^^ literal
    | 2uy -> c.B <- combo &&& 7L
    | 3uy ->
        if c.A <> 0L then
            c.IP <- int literal
    | 4uy -> c.B <- c.B ^^^ c.C
    | 5uy ->
        let x = combo &&& 7L
        c.OutputChecksum <- c.OutputChecksum ^^^ (x <<< (c.Output.Count % 64))
        c.Output.Add(x |> byte)
    //if checksum c.Output <> c.OutputChecksum then failwith "checksum error"
    | 6uy -> c.B <- c.A / (1L <<< int32 combo)
    | 7uy -> c.C <- c.A / (1L <<< int32 combo)
    | _ -> failwith "invalid opCode"

[<TailCall>]
let runProgram c =
    let debugPrint = c.A = 247839002892798L

    while c.IP + 1 < c.Program.Length do
        if debugPrint then
            printfn "%A" c

        instr c.Program[c.IP] c.Program[c.IP + 1] c

    c

let part1 lines =
    let c = parseInput lines
    runProgram c

let part2 lines =
    let c = parseInput lines
    let checksum = checksum c.Program
    let mutable a = 247839002892472L
    let mutable finish = false

    while not finish do
        if a % 1000000L = 0L then
            printfn "%i" a

        c.Output.Clear()
        c.OutputChecksum <- 0L
        c.IP <- 0
        c.A <- a
        c.B <- 0L
        c.C <- 0L
        let r = runProgram c

        if r.OutputChecksum = checksum && r.Output |> Seq.toArray = c.Program then
            finish <- true

        a <- a + 1L

    a - 1L

let sol =
    { Day = 17
      Part1 = solution part1 (_.Output >> Seq.map string >> String.concat ",")
      Part2 = solution part2 (string) }
