open Common
let runAll() =
    runDay day01.sol
    runDay day02.sol
    runDay day03.sol
    runDay day04.sol
    runDay day05.sol
    runDay day06.sol
    runDay day07.sol
    runDay day08.sol
    runDay day09.sol
    runDay day10.sol
    runDay day11.sol
    runDay day12.sol
    runDay day13.sol
    runDay day14.sol
    runDay day15.sol
    runDay day16.sol
    runDay day17.sol
    runDay day18.sol
    runDay day19.sol
    runDay day20.sol
    runDay day21.sol
    runDay day22.sol
    runDay day23.sol
    runDay day24.sol
    runDay day25.sol
 
//Common.benchmark "TOTAL" runAll
Common.benchmark "CURRENT" (fun () -> runDaySample day15.sol)