open Common
let runAll() =
    runDay day01.sol
 

let benchmark f =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    f()
    sw.Stop()
    printfn $"%A{sw.Elapsed}"

benchmark runAll