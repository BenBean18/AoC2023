module Main where

import System.Environment
import qualified Day1
import qualified Day2
import qualified Day3

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
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else if part == "2" then
                    if day == 1 then Day1.part2
                    else if day == 2 then Day2.part2
                    else if day == 3 then Day3.part2
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else putStrLn "Usage: AoC2023 <day> <part || \"v\" for visualization>"
    else putStrLn "Usage: AoC2023 <day> <part || \"v\" for visualization>"