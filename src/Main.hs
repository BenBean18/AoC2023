module Main where

import System.Environment
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6

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
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else if part == "2" then
                    if day == 1 then Day1.part2
                    else if day == 2 then Day2.part2
                    else if day == 3 then Day3.part2
                    else if day == 4 then Day4.part2
                    else if day == 5 then Day5.part2
                    else if day == 6 then Day6.part2
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else if part == "b" then
                    if day == 1 then Day1.benchmark
                    else if day == 2 then Day2.benchmark
                    else if day == 3 then Day3.benchmark
                    else if day == 4 then Day4.benchmark
                    else if day == 6 then Day6.benchmark
                    else putStrLn "Not benchmarked"
                else putStrLn "Usage: AoC2023 <day> <part || \"v\" for visualization>"
    else putStrLn "Usage: AoC2023 <day> <part || \"v\" for visualization>"