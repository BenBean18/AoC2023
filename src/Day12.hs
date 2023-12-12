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

import Data.List.Unique (allUnique)

allCombinationsFor :: String -> [String]
allCombinationsFor [c] = if c == '?' then [".","#"] else [[c]]
allCombinationsFor (c:str) =
    let chars = if c == '?' then ['.','#'] else [c] in concatMap (\ch -> map (ch : ) (allCombinationsFor str)) chars

regexFor :: Int -> String
regexFor i = "\\.+#{" ++ show i ++ "}"

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

-- Part 2

numPossibilitiesForLineUnfolded :: String -> Int
numPossibilitiesForLineUnfolded s =
    let splot = words s
        record = head splot
        combo = map (\s -> read s :: Int) (splitOn "," (last splot))
        (uRecord, uCombo) = unfold record combo in numPossibilities uRecord uCombo

unfold :: String -> [Int] -> (String, [Int])
unfold str combo = (concat (intersperse "?" (replicate 5 str)), concat (replicate 5 combo))

part2' lines = 
    let result = sum $ map numPossibilitiesForLineUnfolded lines in print result

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