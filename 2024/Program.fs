open Common
let runAll() =
    runDay day01.sol
    runDay day02.sol
    runDay day03.sol
 
Common.benchmark "TOTAL" runAll