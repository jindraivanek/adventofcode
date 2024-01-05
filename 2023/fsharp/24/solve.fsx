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
    |> Seq.map (fun [ [ x1; y1; z1 ]; [ x2; y2; z2 ] ] -> (x1, y1, z1), (x2, y2, z2))
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

let maxT = 6L

let validSpeed x1 x2 d1 d2 t1 t2 =
    let a = x1 - x2 + t1 * d1 - t2 * d2
    let b = t1 - t2
    if b <> 0L && a % b = 0L then Some (a / b) else None

let findSpeedForPair t1 ((x1, y1, z1), (dx1, dy1, dz1)) ((x2, y2, z2), (dx2, dy2, dz2)) =
    seq {
        for t2 in 1L .. maxT do
            match validSpeed x1 x2 dx1 dx2 t1 t2, validSpeed y1 y2 dy1 dy2 t1 t2, validSpeed z1 z2 dz1 dz2 t1 t2 with
            | Some dx, Some dy, Some dz -> 
                printfn $"({x1}, {y1}, {z1}) ({x2}, {y2}, {z2}) -> ({dx}, {dy}, {dz}) at {t1} {t2}"
                yield t2, (dx, dy, dz)
            | _ -> ()
    }

let findSpeed xs =
    let rec go s1 t1 h1 = function
        | [] -> seq [s1]
        | h2 :: rest ->
            findSpeedForPair t1 h1 h2 |> Seq.filter (fun (t2, s) -> s1 = None || Some s = s1) |> Seq.collect (fun (t2, s) -> go (Some s) t2 h2 rest) 
    go None 1L (xs |> Seq.head) (xs |> Seq.skip 1 |> Seq.toList)

let absMinBy f xs =
    xs
    |> Seq.map (fun x -> abs (f x), x)
    |> Seq.minBy fst
    |> snd

let findSpeeds() =
    let hails = hails int64
    let h1 = hails |> Seq.head
    let h2 = hails |> Seq.nth 1
    findSpeed hails |> Seq.head
    |> fun it -> printfn $" {h1} {h2} -> {it}"

let part2 =
    findSpeeds()
    0

printfn $"{part1}" //26657
printfn $"{part2}"
