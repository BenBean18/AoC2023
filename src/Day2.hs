module Day2 where

import Utilities
import Data.List.Split

import Data.Text (pack, unpack, replace, isInfixOf)

import Text.Regex.Base
import Text.Regex.PCRE

import Criterion.Main
import System.Environment

data Game = Game { gameId :: Int, maxRed :: Int, maxGreen :: Int, maxBlue :: Int }

parseSubset :: String -> [Int]
parseSubset l = 
    let blueMatches = l =~ "(\\d+) blue" :: [[String]]
        redMatches = l =~ "(\\d+) red" :: [[String]]
        greenMatches = l =~ "(\\d+) green" :: [[String]]
        blue = read (if (length blueMatches) == 0 then "0" else (last $ head blueMatches)) :: Int
        green = read (if (length greenMatches) == 0 then "0" else (last $ head greenMatches)) :: Int
        red = read (if (length redMatches) == 0 then "0" else (last $ head redMatches)) :: Int in
            [red, green, blue]

subsets :: String -> [[Int]]
subsets s = map parseSubset $ (splitOn ";" s)

findMaxRed :: [[Int]] -> Int
findMaxRed subsets = maximum $ map (\s -> s !! 0) subsets

findMaxGreen :: [[Int]] -> Int
findMaxGreen subsets = maximum $ map (\s -> s !! 1) subsets

findMaxBlue :: [[Int]] -> Int
findMaxBlue subsets = maximum $ map (\s -> s !! 2) subsets

parseGame :: String -> Game
parseGame s = 
    let idMatches = s =~ "Game (\\d+)" :: [[String]]
        ourId = read (last (head idMatches)) :: Int
        ourSubsets = subsets s in
        Game { gameId = ourId, maxRed = findMaxRed ourSubsets, maxGreen = findMaxGreen ourSubsets, maxBlue = findMaxBlue ourSubsets }

isValid :: Game -> Bool
isValid Game { maxRed = r, maxGreen = g, maxBlue = b } =
    r <= 12 && g <= 13 && b <= 14

-- Part 1
part1' lines = 
    let games = map parseGame lines in
        print $ sum $ (map gameId (filter isValid games))

power :: String -> Int
power s = 
    let ourSubsets = subsets s in
        findMaxBlue ourSubsets * findMaxRed ourSubsets * findMaxGreen ourSubsets

part1 = do
    lines <- getLines "day2/input.txt"
    part1' lines

-- Part 2
part2' lines = print $ sum $ (map power lines)

part2 = do
    lines <- getLines "day2/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day2.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day2/input.txt"
    time lines