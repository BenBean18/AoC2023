module Day3 where

import Utilities
import Data.List.Split

import Data.List

import Data.Text (pack, unpack, replace, isInfixOf)

import Text.Regex.Base
import Text.Regex.PCRE

import Data.Array ((!))

import qualified Data.Map as Map

coordinatesAroundList :: [(Int, Int)] -> [(Int, Int)]
coordinatesAroundList coordsList = foldl (++) [] (map coordinatesAround coordsList)

coordinatesAround :: (Int, Int) -> [(Int, Int)]
coordinatesAround (x, y) =
    [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]

getSymbol :: [String] -> (Int, Int) -> Char
getSymbol s (x, y) = 
    if x < 0 || y < 0 || x >= (length s) || y >= (length s) then '.'
    else (s !! y) !! x

validNumber :: [String] -> ([Int], Int) -> Bool
validNumber s (xs, y) =
    any (\c -> c /= '.' && not (c `elem` ['0'..'9'])) $ map (getSymbol s) (coordinatesAroundList (map (\x -> (x, y)) xs))

findValidNumbers :: [String] -> Int -> String -> [Int]
findValidNumbers strs i s = 
    let matchArrayList = map (\(a, b) -> [a..a+b-1]) $ map (! 0) $ matchAll (makeRegex "(\\d+)" :: Regex) s
        matchList = map head ((s =~ "(\\d+)") :: [[String]])
        zipped = zip matchArrayList matchList in
            map (\x -> read x :: Int) $ map snd $ filter (\(indices, str) -> validNumber strs (indices, i)) zipped

-- Part 1
part1 = do
    lines <- getLines "day3/input.txt"
    print (sum $ (map (\i -> sum $ findValidNumbers lines i (lines !! i)) [0..length lines - 1]) )

findPartNumbers lines = foldl (++) [] $ map (\i -> findValidNumbers lines i (lines !! i)) [0..length lines-1]

-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

findStars strs i =
    let s = (strs !! i)
        matchArrayList = map (\(a, b) -> [a..a+b-1]) $ map (! 0) $ matchAll (makeRegex "(\\d+)" :: Regex) s
        matchList = map head ((s =~ "(\\d+)") :: [[String]])
        zipped = zip matchArrayList matchList
        allChars = map (\(indices, str) -> map (\coords -> ((getSymbol strs coords), (coords), str)) (coordinatesAroundList (map (\index -> (index, i)) indices))) zipped
        allStars = map (\(a,b,c) -> (b,[(read c :: Int)])) $ concatMap rmdups $ map (filter (\(c, coords, s) -> c == '*')) allChars in Map.unionsWith (++) $ map (\a -> Map.fromList [a]) allStars

findGears strs = Map.filter (\nums -> length nums == 2) (Map.unionsWith (++) (map (findStars strs) [0..length strs-1]))

-- To find a valid gear, it must be adjacent to two part numbers
-- Strategy: for each part number, store adjacent *s
-- Keep a map from star coordinates to number of intersections and the numbers intersecting
-- Filter for intersections == 2 and map multiply numbers etc let's go

-- 75039901 is too low

-- Part 2
part2 = do
    lines <- getLines "day3/input.txt"
    print $ sum $ map product (Map.elems $ findGears lines)