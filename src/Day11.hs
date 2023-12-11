module Day11 where

import Utilities
import Data.List.Split
import Data.List ()
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

-- I think today is an MST

isEmpty :: String -> Bool
isEmpty = all (== '.')

expandRows :: [String] -> [String]
expandRows [] = []
expandRows (x:xs) = if isEmpty x then x : x : expandRows xs else x : expandRows xs

columnAt :: [String] -> Int -> String
columnAt strs i = map (!! i) strs

transpose :: [String] -> [String]
transpose strs = map (columnAt strs) [0..length (head strs)-1]

expandGalaxy :: [String] -> [String]
expandGalaxy strs = expandRows (transpose (expandRows (transpose strs)))

-- Part 1
part1' lines = putStrLn $ concatMap (\s -> s ++ "\n") (expandGalaxy lines)

part1 = do
    lines <- getLines "day11/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day11/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day11.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day11/input.txt"
    time lines