module Day12 where

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

allCombinationsFor :: String -> [String]
allCombinationsFor [c] = if c == '?' then [".","#"] else [[c]]
allCombinationsFor (c:str) =
    let chars = if c == '?' then ['.','#'] else [c] in concatMap (\ch -> map (ch : ) (allCombinationsFor str)) chars

regexFor :: Int -> String
regexFor i = "\\.+#{" ++ show i ++ "}"

fitsPattern :: String -> [Int] -> Bool
fitsPattern record combo = combo == map length (wordsBy (== '.') record)

numPossibilities :: String -> [Int] -> Int
numPossibilities record combo = genericLength (filter id (map (`fitsPattern` combo) (allCombinationsFor record)))

numPossibilitiesForLine :: String -> Int
numPossibilitiesForLine s =
    let splot = words s
        record = head splot
        combo = map (\s -> read s :: Int) (splitOn "," (last splot)) in numPossibilities record combo

-- Part 1
part1' lines =
    let result = sum $ map numPossibilitiesForLine lines in print result

part1 = do
    lines <- getLines "day12/input.txt"
    part1' lines

-- Part 2
-- Try possible combinations splitting on each dot character
-- ...but that is super not optimal on something like this "?????????????? 1,1,1,5"
-- (14 question marks)
-- Maybe compute the # of possibilities for all question marks?
-- And then see what the restrictions are
-- We can also see how many '.' characters there are at minimum by seeing # of groups
-- e.g. with the all ? input, there are 4 groups -> need at least 3 dots
-- which means there are 14-3 = 11 characters we can manipulate
-- 8 of them have to be #s (since 1+1+1+5 = 8)
-- 11-8 = 3 so we now know the number of characters we can manipulate
-- 3 ., 8 #, 3 ?
-- Also, the string ".#" has to appear 3 times (and potentially 4, but it could start with just "#")
-- These things have to be in the output in this order (potentially overlapping)
-- #. (len 2)
-- .#. (len 3)
-- .#. (len 3)
-- .##### (len 5)
-- #. .#. .#. .#####
-- so without overlap it's I#.I#.I#.I#####I
-- 2+3+3+5 - 3 for overlap = 10 characters predetermined
-- There are 4 characters we can manipulate and they go into 4 groups
-- (this will always be true, n = g)
-- https://math.stackexchange.com/questions/47345/number-of-ways-of-distributing-n-identical-objects-among-r-groups
-- (n + g - 1)! / (g - 1)!(n + g - 1 - g + 1)! = (n + g - 1)! / (g - 1)!n! = (2n - 1)! / (n-1)!n!
-- in this case, that's (7!)/(3!)(4!) = 35 arrangements

-- ?###???????? 3,2,1
-- ###. (len 4)
-- .##. (len 4)
-- .# (len 2)
-- 4+4+2 - 2 for overlap = 8 predetermined
-- 12-9 = 3 to manipulate --> 360 arrangements...way too high

-- Ok very simple case
-- ???? 1,1 (#.#. #..# .#.# is 3 possibilities)
-- also ????? 1,1 is 6 possibilities
-- #. (len 2)
-- .# (len 2)
-- I#.I#I
-- 2 + 2 - 1 for overlap = 3 characters predetermined
-- 4 - 3 = 1 that we can change

-- We can insert characters between groups (removing dots to account for overlap)

-- so what are we actually doing
-- 1 char -> 1 group is 0 1
-- 2 characters -> 2 groups is 0 2, 2 0, 

makeBlock :: Int -> String
makeBlock i = "." ++ replicate i '#' ++ "."

makeBlockList :: [Int] -> [String]
makeBlockList combo = 
    let blocks = map (tail . makeBlock) combo in (init blocks ++ [init (last blocks)])

insertAt :: Int -> Int -> [String] -> [String]
insertAt index number strs = 
    if index > 0 then let before = take index strs in (init before ++ [last before ++ (replicate number '#')]) ++ drop index strs
    else let after = drop index strs in ([(replicate number '#') ++ head after]) ++ tail after

-- Put t things in this group
-- Then allocate t-1 things to put in other groups
-- Returns [[# of things where index is group number]]
insertionCombos' :: Int -> Int -> Int -> [[Int]]
insertionCombos' things groups thisGroup = 
    if groups == thisGroup then [[]] else
    concatMap (\t -> map (\m -> t : m) (insertionCombos' (things - t) groups (thisGroup + 1))) [0..things]

insertionCombos :: Int -> Int -> [[Int]]
insertionCombos things groups = 
    let combos = insertionCombos' things groups 0
        filteredCombos = filter (\allocations -> sum allocations == things) combos in filteredCombos

allPossibilities :: String -> [Int] -> [String]
allPossibilities s combo =
    let blocks = makeBlockList combo
        groups = length blocks + 1
        baseLength = length (concat blocks)
        things = length s - baseLength
        insertionGroups = insertionCombos things groups
        insertionGroupsAndIndices = map (\g -> zip g [0..(length g - 1)]) insertionGroups
        possibilities = (map (\g -> (concat $ foldl (\current (things, index) -> {-(trace $ "\n" ++ (concat current) ++ "\n")-} insertAt index things current) blocks g)) insertionGroupsAndIndices) in
            (trace $ show insertionGroupsAndIndices ++ " " ++ show blocks ++ " " ++ show baseLength ++ " " ++ show things ++ " " ++ show (length possibilities)) possibilities

matches :: String -> String -> Bool
matches string condition = (map (\(c, i) -> if c == '?' then string !! i else c) (zip condition [0..length condition-1])) == string

numPossibilitiesForLineUnfolded :: String -> Int
numPossibilitiesForLineUnfolded s =
    let splot = words s
        record = head splot
        combo = map (\s -> read s :: Int) (splitOn "," (last splot))
        (uRecord, uCombo) = unfold record combo in numPossibilities uRecord uCombo

unfold :: String -> [Int] -> (String, [Int])
unfold str combo = (concat (intersperse "?" (replicate 5 str)), concat (replicate 5 combo))

part2' lines =
    let result = sum $ map numPossibilitiesForLineUnfolded lines in print result

part2 = do
    lines <- getLines "day12/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day12.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day12/input.txt"
    time lines