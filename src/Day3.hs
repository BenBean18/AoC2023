module Day3 where

import Utilities
import Data.List.Split

import Data.Text (pack, unpack, replace, isInfixOf)

import Text.Regex.Base
import Text.Regex.PCRE

import Data.Array ((!))

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

-- Part 2
part2 = do
    lines <- getLines "day3/input.txt"
    print "Hi"