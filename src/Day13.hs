module Day13 where

import Utilities
import Data.List.Split (splitOn, chunksOf)
import Data.List ()
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
import Data.Bits

-- Part 1
columnAt :: [String] -> Int -> String
columnAt strs i = map (!! i) strs

transpose :: [String] -> [String]
transpose strs = map (columnAt strs) [0..length (head strs)-1]

-- just the row version since it's easier and we can transpose
-- go from top to bottom and then also bottom to top

-- simpler example: [1,1,1,2,3,4,4,3,2] -- length 9
-- what if we do a stack and a queue
-- (top to bottom version)
-- stack: 1 2 3 4 4 3 2
-- queue: 2 3 4 4 3 2 1
-- (bottom to top version after adding 5 thingies)
-- line of reflection is the middle index
-- floor(length / 2) rows above
-- 2 below, so 9-2-1 above
-- stack: 2 3 4 4 3 2
-- queue: 2 3 4 4 3 2

-- returns # of rows above
-- NOTE: has to be BETWEEN two rows, can't be a row. so we can just assert that length is even
findLineOfReflectionOneWay :: (Eq a, Show a) => [a] -> [a] -> [a] -> Int
findLineOfReflectionOneWay [] _ _ = 0
findLineOfReflectionOneWay (line:lines) stack queue =
    let newStack = stack ++ [line]
        newQueue = line : queue in {-(trace $ show newStack ++ " " ++ show newQueue)-} (
            if (length newStack > 1) && ((.&.) (length newStack) 1 == 0) && newStack == newQueue then length newStack `div` 2
            else findLineOfReflectionOneWay lines newStack newQueue)

findLineOfReflection :: (Eq a, Show a) => [a] -> Int
findLineOfReflection lines =
    let topToBottom = findLineOfReflectionOneWay lines [] []
        bottomToTop = findLineOfReflectionOneWay (reverse lines) [] [] in
            if topToBottom /= 0 && bottomToTop /= 0 then error (show lines ++ "two reflection lines??? top to bottom: " ++ show topToBottom ++ " bottomToTop: " ++ show bottomToTop)
            else if topToBottom /= 0 then topToBottom
            else if bottomToTop /= 0 then (length lines) - bottomToTop
            else 0

summarizeNote :: [String] -> Int
summarizeNote lines =
    let rowsAbove = findLineOfReflection lines
        columnsToLeft = findLineOfReflection (transpose lines) in columnsToLeft + (100 * rowsAbove)

-- 30821 too low :(

part1' lines =
    let summaries = map summarizeNote (splitOn [""] lines)
        result = sum summaries in do
        print result

part1 = do
    lines <- getLines "day13/input.txt"
    part1' lines

-- Part 2
-- https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell
replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace f i [] = []

allPossibleLineSmudges :: String -> Int -> [String]
allPossibleLineSmudges str i =
    if i >= (length str) then [] else let c = str !! i in
    replace (const (if c == '.' then '#' else '.')) i str : allPossibleLineSmudges str (i+1)

allPossibleSmudges :: [String] -> [[String]]
allPossibleSmudges lines = map (chunksOf (length (head lines))) (allPossibleLineSmudges (concat lines) 0)

summarizeNoteWithout :: [String] -> [String] -> Int
summarizeNoteWithout ignore lines = summarizeNote lines - summarizeNote ignore

part2' lines =
    let firstNote = head $ splitOn [""] lines
        summaries = map (summarizeNoteWithout firstNote) (allPossibleSmudges firstNote)
        result = sum (filter (>0) summaries) in do
        putStrLn $ concatMap (\s -> s ++ "\n") (map (concatMap (\s -> s ++ "\n")) $ allPossibleSmudges firstNote)
        print summaries
        print result

part2 = do
    lines <- getLines "day13/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day13.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day13/input.txt"
    time lines