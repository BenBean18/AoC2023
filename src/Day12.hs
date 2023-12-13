{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Day12 where

import Utilities
import Data.List.Split
import Data.List
import Data.Text (pack, unpack, replace, isInfixOf)
import Text.Regex.Base
import Text.Regex.PCRE
import Data.Array ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Criterion.Main
import System.Environment
import Data.Maybe
import qualified Data.MultiSet as MultiSet

import Data.List.Unique (allUnique, uniq)

allCombinationsFor :: String -> [String]
allCombinationsFor [c] = if c == '?' then [".","#"] else [[c]]
allCombinationsFor (c:str) =
    let chars = if c == '?' then ['.','#'] else [c] in concatMap (\ch -> map (ch : ) (allCombinationsFor str)) chars

fitsPattern :: String -> [Int] -> Bool
fitsPattern record combo = combo == map length (wordsBy (== '.') record)

numPossibilities :: String -> [Int] -> Int
numPossibilities record combo = genericLength (filter id (map (`fitsPattern` combo) (allCombinationsFor record)))

numPossibilitiesForLine :: String -> Int
numPossibilitiesForLine s =
    let splot = words s
        record = head splot
        combo = map (\s -> read s :: Int) (splitOn "," (last splot)) in numPossibilities record combo

-- Part 1
part1' lines =
    let result = sum $ map numPossibilitiesForLine lines in print result

part1 = do
    lines <- getLines "day12/input.txt"
    part1' lines

-- Walk along the list from left to right
-- Build up the current combos
-- Ignore previous ones
-- Ignore dots

numValid :: String -> [Int] -> Int -> Int
numValid s [] _ = if all (\x -> x == '.' || x == '?') s then 1 else 0
numValid [] (condition:[]) blockSize = if (blockSize == condition) then 1 else 0
numValid (x:xs) (condition:conditions) currentBlockSize = {-(trace $ show x ++ " " ++ show xs ++ " " ++ show condition ++ " " ++ show currentBlockSize)-} (
    if x == '.' then numValid xs (condition:conditions) 0
    else if x == '#' && (length xs > 0) && (head xs) == '.' then
        -- end of combo (this is #, next is .)
        let currentCombo = currentBlockSize + 1 in
            if condition /= currentCombo then 0 else numValid (tail xs) conditions 0
    else if x == '#' && (length xs > 0) && (head xs) == '?' then
        -- end of combo ('?' == '.')
        {-(trace "hi")-} (let currentCombo = currentBlockSize + 1 in if condition /= currentCombo then 0 else numValid ('.' : tail xs) conditions 0) +
        -- continue combo ('?' == '#')
        numValid ('#' : tail xs) (condition:conditions) (currentBlockSize + 1)
    else if x == '#' then {-(trace "hello")-} numValid xs (condition:conditions) (currentBlockSize + 1)
    else numValid ('.' : xs) (condition:conditions) currentBlockSize + numValid ('#' : xs) (condition:conditions) currentBlockSize)
numValid x y _ = {-(trace $ show x ++ " " ++ show y ++ "----------")-} 0 -- catch

numPossibilitiesForLineUnfolded :: String -> Int
numPossibilitiesForLineUnfolded s =
    let splot = words s
        record = head splot
        combo = map (\s -> read s :: Int) (splitOn "," (last splot))
        (uRecord, uCombo) = unfold record combo in numValid uRecord uCombo 0

unfold :: String -> [Int] -> (String, [Int])
unfold str combo = (concat (intersperse "?" (replicate 5 str)), concat (replicate 5 combo))

part2' lines =
    let result = sum $ map (\s -> (trace s) numPossibilitiesForLineUnfolded s) lines in print result

part2 = do
    lines <- getLines "day12/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day12.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day12/input.txt"
    time lines