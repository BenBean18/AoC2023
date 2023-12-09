module Day9 where

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

differencesList :: [Int] -> [Int]
differencesList ints = zipWith (-) (tail ints) (init ints)

extrapolate :: [Int] -> [Int] -> [Int]
extrapolate thisList derivativeList = {-(trace $ show thisList ++ " " ++ show derivativeList)-} (thisList ++ [last thisList + last derivativeList])

differenceLists :: [Int] -> [[Int]]
differenceLists ints =
    if all (== 0) ints then []
    else 
        let dList = differencesList ints in ints : differenceLists dList

extrapolateHistory :: String -> Int
extrapolateHistory s =
    let nums = map (\w -> (read w :: Int)) (words s)
        lists = differenceLists nums in last (foldr extrapolate (last lists) (init lists))

-- Part 1
part1' lines =
    let extrapolatedValues = map extrapolateHistory lines
        result = sum extrapolatedValues in print result

part1 = do
    lines <- getLines "day9/input.txt"
    part1' lines

-- Part 2
extrapolateBackwards :: [Int] -> [Int] -> [Int]
extrapolateBackwards thisList derivativeList = {-(trace $ show thisList ++ " " ++ show derivativeList)-} (head thisList - head derivativeList) : thisList

extrapolateHistoryBackwards :: String -> Int
extrapolateHistoryBackwards s =
    let nums = map (\w -> (read w :: Int)) (words s)
        lists = differenceLists nums in head (foldr extrapolateBackwards (last lists) (init lists))

part2' lines =
    let extrapolatedValues = map extrapolateHistoryBackwards lines
        result = sum extrapolatedValues in print result

part2 = do
    lines <- getLines "day9/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day9.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day9/input.txt"
    time lines