#r "nuget: Rationals"
open Rationals
#time

//let lines = System.IO.File.ReadAllLines("input")
let lines = System.IO.File.ReadAllLines("sample")

let pos3plus (x1, y1, z1) (x2, y2, z2) = x1 + x2, y1 + y2, z1 + z2
let pos3minus (x1, y1, z1) (x2, y2, z2) = x1 - x2, y1 - y2, z1 - z2
let pos3dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2
let pos3product (x1, y1, z1) (x2, y2, z2) = y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2
let pos3mul (x1, y1, z1) a = x1 * a, y1 * a, z1 * a
let pos3Canonical (x: Rational, y: Rational, z:Rational) = x.CanonicalForm, y.CanonicalForm, z.CanonicalForm

let allPairs xs =
    xs
    |> Seq.mapi (fun i x1 -> xs |> Seq.skip (i+1) |> Seq.map (fun x2 -> x1, x2))
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


let maxT = 10L
let maxD = 3L
let speedVariants =
    seq {
        for dx in 1L .. maxD do
            for dy in 1L .. maxD do
                for dz in 1L .. maxD do
                    yield dx, dy, dz
                    yield -dx, dy, dz
                    yield dx, -dy, dz
                    yield dx, dy, -dz
                    yield -dx, -dy, dz
                    yield dx, -dy, -dz
                    yield -dx, dy, -dz
                    yield -dx, -dy, -dz
    }

let validSpeed x1 x2 d1 d2 t1 t2 =
    let a = x1 - x2 + t1 * d1 - t2 * d2
    let b = t1 - t2
    if b <> 0L && a % b = 0L then Some (a / b) else None

let getTime x1 x2 d1 d2 t1 d =
    let a = x1 - x2 + t1 * d1 - t1 * d
    let b = d2 - d
    if b <> 0L && a % b = 0L then Some (a / b) else None

let getTimeRational x1 x2 d1 d2 t1 d : Rational option =
    let a = x1 - x2 + t1 * d1 - t1 * d
    let b = d2 - d
    if b <> Rational 0 then Some (a / b) else None

let findSpeedForPair t1 ((x1, y1, z1), (dx1, dy1, dz1)) ((x2, y2, z2), (dx2, dy2, dz2)) =
    seq {
        for t2 in 0L .. maxT do
            match validSpeed x1 x2 dx1 dx2 t1 t2, validSpeed y1 y2 dy1 dy2 t1 t2, validSpeed z1 z2 dz1 dz2 t1 t2 with
            | Some dx, Some dy, Some dz -> 
                //printfn $"({x1}, {y1}, {z1}) ({x2}, {y2}, {z2}) -> ({dx}, {dy}, {dz}) at {t1} {t2}"
                yield t2, (dx, dy, dz)
            | _ -> ()
    }

let findSpeed xs =
    let rec go s1 t1 h1 = function
        | [] -> seq [s1]
        | h2 :: rest ->
            findSpeedForPair t1 h1 h2 |> Seq.filter (fun (t2, s) -> s1 = None || Some s = s1) |> Seq.collect (fun (t2, s) -> go (Some s) t2 h2 rest) 
    seq {
        for t1 in 1L .. maxT do
            yield! go None t1 (xs |> Seq.head) (xs |> Seq.skip 1 |> Seq.toList) |> Seq.map (fun d -> t1, d)
    }

let findTimeForPair t1 (dx, dy, dz) ((x1, y1, z1), (dx1, dy1, dz1)) ((x2, y2, z2), (dx2, dy2, dz2)) =
    seq {
        match getTime x1 x2 dx1 dx2 t1 dx, getTime y1 y2 dy1 dy2 t1 dy, getTime z1 z2 dz1 dz2 t1 dz with
        | Some t2x, Some t2y, Some t2z when t2x = t2y && t2y = t2z -> 
            //printfn $"({x1}, {y1}, {z1}) ({x2}, {y2}, {z2}) -> ({dx}, {dy}, {dz}) at {t1} {t2}"
            yield t2x, (dx, dy, dz)
        | _ -> ()
    }

let findTimeForPairR t1 (dx, dy, dz) ((x1, y1, z1), (dx1, dy1, dz1)) ((x2, y2, z2), (dx2, dy2, dz2)) =
        let a = -x1 - y1 - z1 + x2 + y2 + z2 + t1 * (-dx1 - dy1 - dz1 + dx + dy + dz)
        let b = dx2 + dy2 + dz2 - dx - dy - dz
        if b <> Rational 0 then Some (-a / b) else None

let findSpeedWithTime xs =
    let rec go s1 t1 h1 = function
        | [] -> seq [s1]
        | h2 :: rest ->
            match s1 with
            | None -> 
                speedVariants |> Seq.collect (fun d -> findTimeForPair t1 d h1 h2) |> Seq.collect (fun (t2, s) -> 
                    //printfn $"({h1}, {h2}) ({t1}, {t2}) {s}"
                    go (Some s) t2 h2 rest)
            | Some d ->
                findTimeForPair t1 d h1 h2 |> Seq.collect (fun (t2, s) -> go (Some s) t2 h2 rest)
    seq {
        for t1 in 1L .. maxT do
            yield! go None t1 (xs |> Seq.head) (xs |> Seq.skip 1 |> Seq.toList) |> Seq.map (Option.map (fun d -> t1, d))
    }

let findTimesR xs =
    let rec go acc s t1 h1 = function
        | [] -> 
            let xs = acc |> List.map (fun (r: Rational) -> r.CanonicalForm) |> List.rev 
            if xs |> List.forall (fun r -> (* r.FractionPart = Rational 0 && *) r.Sign > 0) then Some xs else None
        | h2 :: rest ->
            findTimeForPairR t1 s h1 h2 |> Option.bind (fun t2 -> go (t2 :: Seq.toList acc) s t2 h2 rest)
    seq {
        for (dx, dy, dz) in speedVariants do
            printfn $"({dx}, {dy}, {dz})"
            let t1 = 10L
            let s = Rational dx, Rational dy, Rational dz
            yield 
                go [Rational t1] s (Rational t1) (xs |> Seq.head) (xs |> Seq.skip 1 |> Seq.toList)
                |> Option.map (fun rs -> 
                    rs |> Seq.iter (fun r -> printfn $"{r.CanonicalForm} {float r}")
                    Rational t1, s)
    }

let absMinBy f xs =
    xs
    |> Seq.map (fun x -> abs (f x), x)
    |> Seq.minBy fst
    |> snd

let linePlaneIntersection p0 pv l1 l2  =
    let u = pos3minus l2 l1
    let dot = pos3dot pv u

    if abs(dot) > Rational 0 then
        let w = pos3minus l1 p0
        let fac = -pos3dot pv w / dot
        let u = pos3mul u fac
        Some (pos3plus p0 u)
    else None

let planeNormal point line1 line2 =
    let u = pos3minus line2 line1
    let v = pos3minus point line1
    let n = pos3product u v
    n

let findT1 hails =
    let ((x1, y1, z1), (dx1, dy1, dz1)) as h1 = hails |> Seq.head
    let tHails = hails |> Seq.skip 1 |> Seq.map (fun ((x2, y2, z2), (dx2, dy2, dz2)) -> (x2 - x1, y2 - y1, z2 - z1), (dx2 - dx1, dy2 - dy1, dz2 - dz1))
    let ((x2, y2, z2), (dx2, dy2, dz2)) as h2 = tHails |> Seq.head
    let ((x3, y3, z3), (dx3, dy3, dz3)) as h3 = tHails |> Seq.skip 1 |> Seq.head
    let ((x4, y4, z4), (dx4, dy4, dz4)) as h4 = tHails |> Seq.skip 2 |> Seq.head
    let zero = Rational 0, Rational 0, Rational 0
    let p1 = (x2, y2, z2)
    let p2 = pos3plus p1 (dx2, dy2, dz2)
    tHails |> Seq.iter (fun (d, s) -> printfn $"({d}) ({s})")
    let n = planeNormal zero (fst h2) (pos3plus (fst h2) (snd h2))
    let (Some i1) = linePlaneIntersection zero n (fst h3) (fst h3 |> pos3plus (snd h3)) |> Option.map pos3Canonical
    let (Some i2) = linePlaneIntersection zero n (fst h4) (fst h4 |> pos3plus (snd h4)) |> Option.map pos3Canonical
    printfn $"({i1}) ({i2})"
    let d = pos3minus i1 i2 |> pos3Canonical
    printfn $"({d})"
    let r = pos3plus (snd h1) d |> pos3Canonical
    printfn $"({r})"
    r

let findSpeeds() =
    let hails = hails Rational
    let ((x1, y1, z1), (dx1, dy1, dz1)) as h1 = hails |> Seq.head
    findT1 hails
    // hails |> Seq.collect (fun h1 -> 
    //     let hs = h1 :: (hails |> List.filter (fun h -> h <> h1))
    //     //printfn $"findSpeedWithTime: ({h1})"
    //     findTimesR hs) |> Seq.toList
    //findTimesR hails |> Seq.choose id |> Seq.last //|> (fun (t1, rs) -> printfn "{t1}"; rs |> Seq.iter (fun r -> printfn $"{r.CanonicalForm}"); t1, rs)
    //|> fun (t1, (dx, dy, dz)) -> x1 + t1 * dx1 - t1 * dx, y1 + t1 * dy1 - t1 * dy, z1 + t1 * dz1 - t1 * dz
let part2 =
    findSpeeds()
    //let x,y,z = findSpeeds()
    //x + y + z
    0

printfn $"{part1}" //26657
printfn $"{part2}"
