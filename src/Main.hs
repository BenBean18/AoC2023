module Main where

import System.Environment
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then
        let part = args !! 1
            day = (read (args !! 0) :: Int) in
                if part == "1" then
                    if day == 1 then Day1.part1
                    else if day == 2 then Day2.part1
                    else if day == 3 then Day3.part1
                    else if day == 4 then Day4.part1
                    else if day == 5 then Day5.part1
                    else if day == 6 then Day6.part1
                    else if day == 7 then Day7.part1
                    else if day == 8 then Day8.part1
                    else if day == 9 then Day9.part1
                    else if day == 10 then Day10.part1
                    else if day == 11 then Day11.part1
                    else if day == 12 then Day12.part1
                    else if day == 13 then Day13.part1
                    else if day == 14 then Day14.part1
                    else if day == 15 then Day15.part1
                    else if day == 16 then Day16.part1
                    else if day == 17 then Day17.part1
                    else if day == 18 then Day18.part1
                    else if day == 19 then Day19.part1
                    else if day == 20 then Day20.part1
                    else if day == 21 then Day21.part1
                    else if day == 22 then Day22.part1
                    else if day == 23 then Day23.part1
                    else if day == 24 then Day24.part1
                    else if day == 25 then Day25.part1
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else if part == "2" then
                    if day == 1 then Day1.part2
                    else if day == 2 then Day2.part2
                    else if day == 3 then Day3.part2
                    else if day == 4 then Day4.part2
                    else if day == 5 then Day5.part2
                    else if day == 6 then Day6.part2
                    else if day == 7 then Day7.part2
                    else if day == 8 then Day8.part2
                    else if day == 9 then Day9.part2
                    else if day == 10 then Day10.part2
                    else if day == 11 then Day11.part2
                    else if day == 12 then Day12.part2
                    else if day == 13 then Day13.part2
                    else if day == 14 then Day14.part2
                    else if day == 15 then Day15.part2
                    else if day == 16 then Day16.part2
                    else if day == 17 then Day17.part2
                    else if day == 18 then Day18.part2
                    else if day == 19 then Day19.part2
                    else if day == 20 then Day20.part2
                    else if day == 21 then Day21.part2
                    else if day == 22 then Day22.part2
                    else if day == 23 then Day23.part2
                    else if day == 24 then Day24.part2
                    else if day == 25 then Day25.part2
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else if part == "b" then
                    if day == 1 then Day1.benchmark
                    else if day == 2 then Day2.benchmark
                    else if day == 3 then Day3.benchmark
                    else if day == 4 then Day4.benchmark
                    else if day == 5 then Day5.benchmark
                    else if day == 6 then Day6.benchmark
                    else if day == 7 then Day7.benchmark
                    else if day == 8 then Day8.benchmark
                    else if day == 9 then Day9.benchmark
                    else if day == 10 then Day10.benchmark
                    else if day == 11 then Day11.benchmark
                    else if day == 12 then Day12.benchmark
                    else if day == 13 then Day13.benchmark
                    else if day == 14 then Day14.benchmark
                    else if day == 15 then Day15.benchmark
                    else if day == 16 then Day16.benchmark
                    else if day == 17 then Day17.benchmark
                    else if day == 18 then Day18.benchmark
                    else if day == 19 then Day19.benchmark
                    else if day == 20 then Day20.benchmark
                    else if day == 21 then Day21.benchmark
                    else if day == 22 then Day22.benchmark
                    else if day == 23 then Day23.benchmark
                    else if day == 24 then Day24.benchmark
                    else if day == 25 then Day25.benchmark
                    else putStrLn "Not benchmarked"
                else putStrLn "Usage: AoC2023 <day> <part || \"v\" for visualization>"
    else putStrLn "Usage: AoC2023 <day> <part || \"v\" for visualization>"