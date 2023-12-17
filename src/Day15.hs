module Day15 where

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
import Data.Char (ord)

-- Part 1
hash' :: String -> Int -> Int
hash' [] currentValue = currentValue
hash' (c:str) currentValue = let newValue = ((currentValue + (ord c)) * 17) `mod` 256 in
    hash' str newValue 

hash :: String -> Int
hash str = hash' str 0

part1' lines =
    let initSequence = head lines
        result = sum (map hash (splitOn "," initSequence)) in print result

part1 = do
    lines <- getLines "day15/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day15/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day15.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day15/input.txt"
    time lines