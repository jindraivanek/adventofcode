#r "nuget: Rationals"
#r "nuget: MathNet.Numerics.FSharp"
#r "nuget: Flips"
open Rationals
open MathNet.Numerics.LinearAlgebra
open Flips
open Flips.Types
open Flips.SliceMap
#time

//let lines = System.IO.File.ReadAllLines("input")
let lines = System.IO.File.ReadAllLines("sample")

let allPairs xs =
    xs
    |> Seq.mapi (fun i x1 -> xs |> Seq.skip i |> Seq.map (fun x2 -> x1, x2))
    |> Seq.concat

let (=~) x y = abs (x - y) < 0.00001
let (<~) x y = y - x > 0.00001
let (<=~) x y = x =~ y || x <~ y

let num (x: int64) = Rational x

let inline hails conv =
    lines
    |> Seq.map (fun l ->
        l.Split('@')
        |> Seq.map (fun s -> s.Split(", ") |> Seq.map (fun x -> int64 x |> conv) |> Seq.toList)
        |> Seq.toList)
    |> Seq.map (fun [ [ x1; y1; z1 ]; [ x2; y2; z2 ] ] -> (x1, y1, z1), (x1 + x2, y1 + y2, z1 + z2))
    |> Seq.toList

let to2dLine ((x1, y1, _), (x2, y2, _)) = ((x1, y1), (x2, y2))

let isIntersectionInFuture ((x1, y1), (x2, y2)) (x, y) =
    let dx = x2 - x1
    let dy = y2 - y1
    sign (x - x1) = sign dx && sign (y - y1) = sign dy

let linesIntersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
    let d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

    if d = num 0 then
        None
    else
        let a = x1 * y2 - y1 * x2
        let b = x3 * y4 - y3 * x4
        let x = (a * (x3 - x4) - (x1 - x2) * b) / d
        let y = (a * (y3 - y4) - (y1 - y2) * b) / d
        if isIntersectionInFuture ((x1, y1), (x2, y2)) (x, y) && isIntersectionInFuture ((x3, y3), (x4, y4)) (x, y) then
            //printfn $"({x1}, {y1}) ({x2}, {y2}) ({x3}, {y3}) ({x4}, {y4}) -> ({x}, {y}) [{d}, {a}, {b}]"
            Some(x, y)
        else None

let isBetween (x1, y1) (x2, y2) (x, y) =
    x1 <= x && x <= x2 && y1 <= y && y <= y2

let intersectionInTestArea t1 t2 =
    hails Rational
    |> Seq.map to2dLine
    |> allPairs
    |> Seq.choose (fun (h1, h2) -> linesIntersection h1 h2)
    |> Seq.filter (isBetween (t1, t1) (t2, t2))
    |> Seq.length

let part1 = 
    //intersectionInTestArea 7 27
    intersectionInTestArea (num 200000000000000L) (num 400000000000000L)

let linEqSystem() =
    let cols = (hails float |> Seq.length) + 3
    let mkRow indexCoefs rhs = 
        let m = Map.ofList indexCoefs
        [ for i in 1..cols -> Map.tryFind i m |> Option.defaultValue (0.0) ], rhs
    let mkSystem rows =
        rows |> List.unzip |> fun (xs, ys) -> matrix xs, vector ys
    let model =
        hails float
        |> Seq.mapi (fun i ((x1,y1,z1),(x2,y2,z2)) -> [
            mkRow [1, 1.; 4+i, -(x2-x1-1.)] x1
            mkRow [2, 1.; 4+i, -(y2-y1-1.)] y1
            mkRow [3, 1.; 4+i, -(z2-z1-1.)] z1 ]
        ) |> Seq.collect id |> Seq.toList
    let A, b = mkSystem model
    printfn $"{A} = {b}"
    A.Solve(b)

//printfn $"{linEqSystem}"

let n = (hails float |> Seq.length)
let linModel dx dy dz =
    let m = n * n
    let BIG = 1e8    
    let dims = ['X'; 'Y'; 'Z']

    let xi = hails float |> Seq.mapi (fun i ((x,y,z),_) -> [('X', i), x; ('Y', i), y; ('Z', i), z]) |> Seq.collect id |> SMap2.ofSeq
    let offset = SMap2.toSeq xi |> Seq.map snd |> Seq.min
    printfn $"Offset: {offset}"
    let xi = xi |> SMap2.toSeq |> Seq.map (fun (k, x) -> k, x - offset) |> SMap2.ofSeq
    printfn $"max xi: {SMap2.toSeq xi |> Seq.map snd |> Seq.max}"
    let dxi = hails float |> Seq.mapi (fun i ((x1,y1,z1),(x2,y2,z2)) -> [('X', i), x2-x1; ('Y', i), y2-y1; ('Z', i), z2-z1]) |> Seq.collect id |> SMap2.ofSeq

    let x = DecisionBuilder "x" { for _ in dims -> Integer (-infinity, infinity) } |> SMap.ofSeq
    let ts = DecisionBuilder "ts" { for _ in [0..n-1] -> Integer (0.0, infinity) } |> SMap.ofSeq
    let dxbp = DecisionBuilder "dxbp" { for _ in dims do for _ in [1..m] -> Boolean } |> SMap2.ofSeq
    let dxbn = DecisionBuilder "dxbn" { for _ in dims do for _ in [1..m] -> Boolean } |> SMap2.ofSeq

    let dxtsp = DecisionBuilder "dxtsp" { for _ in dims do for _ in [1..m] do for _ in [0..n-1] -> Integer (0.0, infinity) } |> SMap3.ofSeq
    let dxtsn = DecisionBuilder "dxtsn" { for _ in dims do for _ in [1..m] do for _ in [0..n-1] -> Integer (0.0, infinity) } |> SMap3.ofSeq

    let ep = DecisionBuilder "ep" { for _ in dims do for _ in [0..n-1] -> Continuous(0.0, infinity) } |> SMap2.ofSeq
    let en = DecisionBuilder "en" { for _ in dims do for _ in [0..n-1] -> Continuous(0.0, infinity) } |> SMap2.ofSeq

    let consDx = ConstraintBuilder "dx" { for d in dims -> sum dxbp.[d, All] + sum dxbn.[d, All] == 1.0 } 
    let consDxtsp1 = ConstraintBuilder "dxtsp1" { for d in dims do for i in [1..m] do for j in [0..n-1] -> dxtsp[d,i,j] <== BIG * dxbp.[d,i] }
    let consDxtsn1 = ConstraintBuilder "dxtsn1" { for d in dims do for i in [1..m] do for j in [0..n-1] -> dxtsn[d,i,j] <== BIG * dxbn.[d,i] }
    let consDxtsp2 = ConstraintBuilder "dxtsp2" { for d in dims do for i in [1..m] do for j in [0..n-1] -> dxtsp[d,i,j] - (1.0 - dxbp.[d,i]) * BIG <== float i * ts.[j] }
    let consDxtsn2 = ConstraintBuilder "dxtsn2" { for d in dims do for i in [1..m] do for j in [0..n-1] -> dxtsn[d,i,j] - (1.0 - dxbn.[d,i]) * BIG <== float i * ts.[j] }
    let consDxtsp3 = ConstraintBuilder "dxtsp3" { for d in dims do for i in [1..m] do for j in [0..n-1] -> dxtsp[d,i,j] + (1.0 - dxbp.[d,i]) * BIG >== float i * ts.[j] }
    let consDxtsn3 = ConstraintBuilder "dxtsn3" { for d in dims do for i in [1..m] do for j in [0..n-1] -> dxtsn[d,i,j] + (1.0 - dxbn.[d,i]) * BIG >== float i * ts.[j] }

    let consX = ConstraintBuilder "x" { for d in dims do for i in [0..n-1] -> xi.[d,i] + ts[i] * dxi[d,i] == x[d] + sum dxtsp[d,All, i] - sum dxtsn[d,All, i] }

    let objective = Objective.create "obj" Minimize (sum ep.[All, All] + sum en.[All, All])
    let model = 
        Model.create objective
        |> Model.addConstraints consDx
        |> Model.addConstraints consDxtsp1
        |> Model.addConstraints consDxtsn1
        |> Model.addConstraints consDxtsp2
        |> Model.addConstraints consDxtsn2
        |> Model.addConstraints consDxtsp3
        |> Model.addConstraints consDxtsn3
        |> Model.addConstraints consX
    //let settings = { Settings.basic with SolverType = SolverType.CBC; WriteLPFile = Some "day24.lp"}
    let settings = Settings.basic
    printfn "Solving..."
    match Solver.solve settings model with
    | Optimal sol ->
        sol.DecisionResults |> Map.filter (fun _ x -> x <> 0.0) |> Map.toSeq |> Seq.map (fun (k, x) -> k.Name, x) |> Seq.sort |> Seq.iter (printfn "%A")
        sol.ObjectiveResult |> int64 |> Some
    | e -> 
        printfn $"Error: {e}"
        None

let lmLoop() = 
    seq {
    for dx in [1..n] do
        for dy in [1..n] do
            for dz in [1..n] do
                yield linModel dx dy dz
                yield linModel -dx dy dz
                yield linModel dx -dy dz
                yield linModel dx dy -dz
                yield linModel -dx -dy dz
                yield linModel dx -dy -dz
                yield linModel -dx dy -dz
                yield linModel -dx -dy -dz
    } |> Seq.choose id |> Seq.head

let lm() = linModel 1 1 1
//printfn $"LM: {lm()}"

let rec gcd2 a b = 
    if b = 0L then a else gcd2 b (a % b)

let gcd xs = xs |> Seq.fold gcd2 0L

let lcm xs = xs |> Seq.fold (fun acc x -> (acc * x) / gcd2 acc x) 1L

let dxs = hails int64 |> Seq.map (fun ((x1,y1,z1),(x2,y2,z2)) -> x2-x1) |> Seq.toList

printfn "%A" (gcd dxs)

let equations =
    hails int64
    |> Seq.mapi (fun i ((x1,y1,z1),(x2,y2,z2)) -> [
        let i = i + 1
        let dx = x2-x1
        let dy = y2-y1
        let dz = z2-z1
        //sprintf "x(1) + t(%d) * d(1) = %d + %d * t(%d);" i x1 dx i
        sprintf "y(%d) = ((%d - x(1)) / ((x(4) - %d)) + (%d - x(2)) / ((x(5) - %d)) + (%d - x(3)) / ((x(6) - %d))) - 3 * x(%d);" i x1 dx y1 dy z1 dz (i+6)
         ]
    ) |> Seq.collect id 
    |> Seq.toList

equations |> Seq.iter (printfn "%s")

let part2 =
    // let n = num hails.Length
    // let sumXYZ = hails |> Seq.map (fun ((x, y, z), _) -> x + y + z) |> Seq.reduce (+)
    // (sumXYZ / n)
    //int64 linEqSystem.[0] + int64 linEqSystem.[1] + int64 linEqSystem.[2]
    0

printfn $"{part1}" //26657
printfn $"{part2}"
