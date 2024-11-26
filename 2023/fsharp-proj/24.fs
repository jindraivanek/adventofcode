//#r "nuget: Rationals"
module day24
open Rationals

let lines = System.IO.File.ReadAllLines($"%s{__SOURCE_DIRECTORY__}/../../input/2023/24.txt")
//let lines = System.IO.File.ReadAllLines("sample")

let pos3plus (x1, y1, z1) (x2, y2, z2) = x1 + x2, y1 + y2, z1 + z2
let pos3minus (x1, y1, z1) (x2, y2, z2) = x1 - x2, y1 - y2, z1 - z2
let pos3dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2
let pos3product (x1, y1, z1) (x2, y2, z2) = y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2
let pos3mul (x1, y1, z1) a = x1 * a, y1 * a, z1 * a
let pos3div (x1, y1, z1) (dx, dy, dz) = 
    let safeDiv x y = if y = Rational 0 then x else x / y
    safeDiv x1 dx, safeDiv y1 dy, safeDiv z1 dz
let pos3Canonical (x: Rational, y: Rational, z:Rational) = x.CanonicalForm, y.CanonicalForm, z.CanonicalForm

let allPairs xs =
    xs
    |> Seq.mapi (fun i x1 -> xs |> Seq.skip (i+1) |> Seq.map (fun x2 -> x1, x2))
    |> Seq.concat

let num (x: int64) = Rational x

let hails =
    lines
    |> Seq.map (fun l ->
        l.Split('@')
        |> Seq.map (fun s -> s.Split(", ") |> Seq.map (fun x -> int64 x |> Rational) |> Seq.toList)
        |> Seq.toList)
    |> Seq.map (fun [ [ x1; y1; z1 ]; [ x2; y2; z2 ] ] -> (x1, y1, z1), (x2, y2, z2))
    |> Seq.toList

let to2dLine ((x1, y1, _), (x2, y2, _)) = ((x1, y1), (x1+x2, y1+y2))

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
            //printfn $"({x1}, {y1}) ({x2}, {y2}) ({x3}, {y3}) ({x4}, {y4}) -> ({float x}, {float y}) [{d}, {a}, {b}]"
            Some(x, y)
        else None

let isBetween (x1, y1) (x2, y2) (x, y) =
    x1 <= x && x <= x2 && y1 <= y && y <= y2

let intersectionInTestArea t1 t2 =
    hails
    |> Seq.map to2dLine
    |> allPairs
    |> Seq.choose (fun (h1, h2) -> linesIntersection h1 h2)
    |> Seq.filter (isBetween (t1, t1) (t2, t2))
    |> Seq.length

let part1 = 
    //intersectionInTestArea (num 7) (num 27)
    intersectionInTestArea (num 200000000000000L) (num 400000000000000L)

let linePlaneIntersection p0 pv l1 l2  =
    let u = pos3minus l2 l1
    let dot = pos3dot pv u

    if abs(dot) > Rational 0 then
        let w = pos3minus l1 p0
        let fac = -pos3dot pv w / dot
        let u = pos3mul u fac
        let p = pos3plus l1 u |> pos3Canonical
        //printfn $"linePlaneIntersection {p0} {pv} {l1} {l2} -> {p} ({dot} {fac} {u})"
        Some p
    else None

let planeNormal point line1 line2 =
    let u = pos3minus line1 line2
    let v = pos3minus point line2
    let n = pos3product u v
    //printfn $"planeNormal {point} {line1} {line2} -> {n}"
    n

let solve hails =
    let (h1p, h1d) as h1 = hails |> Seq.head
    let tHails = hails |> Seq.skip 1 |> Seq.map (fun (p2, d2) -> pos3minus p2 h1p, pos3minus d2 h1d)
    let (h2p, h2d) = tHails |> Seq.head
    let (h3p, h3d) = tHails |> Seq.skip 1 |> Seq.head
    let (h4p, h4d) = tHails |> Seq.skip 2 |> Seq.head
    let zero = Rational 0, Rational 0, Rational 0
    //tHails |> Seq.iter (fun (d, s) -> printfn $"({d}) ({s})")
    let n = planeNormal zero h2p (pos3plus h2p h2d)
    let (Some i1) = linePlaneIntersection zero n h3p (h3p |> pos3plus h3d) |> Option.map pos3Canonical
    let (Some i2) = linePlaneIntersection zero n h4p (h4p |> pos3plus h4d) |> Option.map pos3Canonical
    //printfn $"{n} ({i1}) ({i2})"
    let d3 = pos3div (pos3minus i1 h3p) h3d |> pos3Canonical
    let d4 = pos3div (pos3minus i2 h4p) h4d |> pos3Canonical
    let getTime d = d |> fun (x, y, z) -> [x; y; z] |> List.find (fun r -> r <> Rational 0)
    let t3 = getTime d3
    let t4 = getTime d4
    let d = pos3mul (pos3minus i1 i2) (Rational 1 / (t3-t4)) |> pos3Canonical
    //printfn $"({t3} {t4}) {d}"
    let r = pos3mul d -t3 |> pos3plus i1 |> pos3plus h1p |> pos3Canonical
    //printfn $"({r})"
    r

let part2 =
    let x,y,z = solve hails
    x + y + z

printfn $"{part1}" //26657
printfn $"{part2}" //828418331313365
