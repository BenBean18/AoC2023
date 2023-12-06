module Day5 where

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

import qualified Day5_part2 (part2')

-- Idea: Each map is a function that outputs Optional[Destination] for a source number
-- Then we store a LOT less data
type AlMapMaybe = Int -> Maybe Int
type AlMap = Int -> Int

emptyMaybeMap :: AlMapMaybe
emptyMaybeMap i = Nothing

emptyMap :: AlMap
emptyMap i = i

-- Format: destination source range_length
parseLine :: String -> AlMapMaybe -> AlMapMaybe
parseLine s m =
    let splot = splitOn " " s
        dst = read (head splot) :: Int
        src = read (splot !! 1) :: Int
        len = read (splot !! 2) :: Int in
            (
                \i ->
                    let current = m i in
                        if isJust current then current
                        else if i >= src && i < src + len then Just (dst + (i - src))
                        else Nothing
            )

parseLines :: String -> AlMap
parseLines s i = let maybeInt = foldl (flip parseLine) emptyMaybeMap (splitOn "\n" s) i in
                     if isJust maybeInt then fromJust maybeInt else i

parseSeeds :: String -> [Int]
parseSeeds str = map (\s -> (read (head s) :: Int)) (str =~ "(\\d+)" :: [[String]])

combineMaps :: [AlMap] -> AlMap
combineMaps = foldl (\existing new -> (\i -> (new (existing i)))) emptyMap

part1' lines = print "Hi"

-- Part 1
part1 = do
    lines <- getLines "day5/input.txt"
    text <- getText "day5/input.txt"
    let seeds = parseSeeds (head lines)
    let maps = map parseLines (tail (map (head . splitOn "\n\n") (splitOn ":\n" text)))
    let combined = combineMaps maps
    print $ minimum $ map combined seeds
    part1' lines

part2' = Day5_part2.part2'

-- Part 2
part2 = do
    lines <- getLines "day5/input.txt"
    part2' lines