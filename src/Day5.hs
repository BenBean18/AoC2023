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

part2' lines = print "Hi"

-- better way might be to represent as a "tree" of ranges or something? solve for intersections

makeRange :: [Int] -> [Int]
makeRange [a,b] = [a..a+b]

parseSeeds2 :: String -> [Int]
parseSeeds2 str = concatMap (\s -> makeRange (map (\i -> (read i :: Int)) (splitOn " " (head s)))) (str =~ "(\\d+) (\\d+)" :: [[String]])

-- Thinking more
-- As we build up the ranges we can keep the input as a range and just take the intersection, keep a list of ranges
-- Not exactly sure how to implement but range intersection is 4 compares instead of checking every single value

-- Or start at minimum location and work way up or something, idk and have to study for finals

-- Part 2
part2 = do
    lines <- getLines "day5/input.txt"
    text <- getText "day5/input.txt"
    let seeds = parseSeeds2 (head lines)
    let maps = map parseLines (tail (map (head . splitOn "\n\n") (splitOn ":\n" text)))
    let combined = combineMaps maps
    print $ minimum $ map combined seeds
    part2' lines