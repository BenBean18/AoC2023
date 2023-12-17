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
part2' lines = print "Hi"

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