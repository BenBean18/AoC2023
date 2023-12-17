module Day14 where

import Utilities
import Data.List.Split
import Data.List (elemIndices)
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

-- Part 1

-- .O...#.O.O is a good test
-- basically while there is empty space to the left, move the O one to the left
shiftUpOne :: String -> Int -> String
shiftUpOne str i = if i == 0 then str else
    let thisChar = str !! i
        prevChar = str !! (i-1) in
            if thisChar /= 'O' then str else
            if prevChar == '.' then shiftUpOne ((take (i-1) str) ++ (thisChar:[prevChar]) ++ (drop (i+1) str)) (i-1) else str

shiftUp :: String -> String
shiftUp s = foldl shiftUpOne s [0..(length s - 1)]

-- heck yeah we like transposing
columnAt :: [String] -> Int -> String
columnAt strs i = map (!! i) strs

transpose :: [String] -> [String]
transpose strs = map (columnAt strs) [0..length (head strs)-1]

loadFor :: [String] -> Int -> Int
loadFor lines i = (length lines - i) * length (elemIndices 'O' (lines !! i))

part1' lines =
    let shiftedUp = transpose (map shiftUp (transpose lines))
        loads = map (loadFor shiftedUp) [0..length lines - 1] in do
            print $ sum loads

part1 = do
    lines <- getLines "day14/input.txt"
    part1' lines

-- Part 2
shiftDownOne str = shiftUpOne (reverse str)

shiftDown str = reverse (shiftUp (reverse str))

-- probably some memoization here
-- or...gasp...*cycle* detection
-- i'll work on this tomorrow, i'm tired
spinCycle lines = map shiftDown (transpose (map shiftDown (transpose (map shiftUp (transpose (map shiftUp (transpose lines)))))))

spin n lines = foldl (\s _ -> spinCycle s) lines (replicate n 0)

-- 108409 too high

part2' lines = --print $ spin 1000 lines
    let spins = foldl (\list _ -> list ++ [spinCycle (last list)]) [lines] [0..1000]
        cycles = map (\i -> elemIndices (spins !! i) spins) [0..1000]
        firstCycle = head (filter (\l -> length l > 1) cycles)
        cycleStart = (head firstCycle)
        period = (firstCycle !! 1) - (firstCycle !! 0)
        offset = (1000000000 - (head firstCycle)) `mod` period
        final = spin offset (spins !! cycleStart)
        loads = map (loadFor final) [0..length lines - 1] in do
            print $ firstCycle
            print $ sum loads

part2 = do
    lines <- getLines "day14/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day14.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day14/input.txt"
    time lines