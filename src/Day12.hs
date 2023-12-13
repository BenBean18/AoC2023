{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
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

import Data.List.Unique (allUnique, uniq)

import Data.Function.Memoize

import Data.IORef
import GHC.IO.Unsafe (unsafePerformIO)
import Data.Functor.Identity (Identity)

allCombinationsFor :: String -> [String]
allCombinationsFor [c] = if c == '?' then [".","#"] else [[c]]
allCombinationsFor (c:str) =
    let chars = if c == '?' then ['.','#'] else [c] in concatMap (\ch -> map (ch : ) (allCombinationsFor str)) chars

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

-- Walk along the list from left to right
-- Build up the current combos
-- Ignore previous ones
-- Ignore dots

{-# NOINLINE memo_table #-}
memo_table :: IORef (Map.Map (String, Int, [Int]) Int)
memo_table = unsafePerformIO (newIORef mempty)

numValid :: String -> Int -> [Int] -> Int
numValid s 0 [] = if all (\x -> x == '.' || x == '?') s then 1 else 0
numValid [] charsLeft [] = if (charsLeft == 0) then 1 else 0
numValid (x:xs) charsLeft conditions =
    if charsLeft < 0 then 0 else 
    (trace $ show x ++ " " ++ show xs ++ " " ++ show charsLeft) (
        (if x == '.' then numValidStored xs charsLeft conditions
        else if x == '#' && (length xs > 0) && (head xs) == '.' then
            -- end of combo (this is #, next is .)
            if charsLeft /= 1 then 0 else numValidStored (tail xs) (if (length conditions) > 0 then head conditions else 0) (if (length conditions) > 0 then tail conditions else [])
        else if x == '#' && (length xs > 0) && (head xs) == '?' then
            -- end of combo ('?' == '.')
            {-(trace "hi")-} (if charsLeft /= 1 then 0 else numValidStored (tail xs) (if (length conditions) > 0 then head conditions else 0) (if (length conditions) > 0 then tail conditions else [])) +
            -- continue combo ('?' == '#')
            numValidStored ('#' : tail xs) (charsLeft - 1) conditions
        else if x == '#' then {-(trace "hello")-} numValidStored xs (charsLeft - 1) conditions
        else numValidStored ('.' : xs) charsLeft conditions + numValidStored ('#' : xs) charsLeft conditions))
numValid x a y = {-(trace $ show x ++ " " ++ show y ++ "----------")-} 0 -- catch

numValidStored' :: String -> Int -> [Int] -> Int
numValidStored' a1 a2 a3 =
    unsafePerformIO $ do
        currentTable <- (readIORef memo_table)
        let returnedGood = Map.findWithDefault (-10) (a1, a2, a3) currentTable
        let returned = if returnedGood /= -10 then returnedGood else numValid a1 a2 a3
        let newMemoTable = Map.insert (a1,a2,a3) returned currentTable
        -- putStrLn "wrote io ref"
        -- print (a1,a2)
        print (Map.size newMemoTable)
        writeIORef memo_table newMemoTable
        return returned

numValidStored a b c = numValidStored' a b c

-- ideally want to memoize (charsLeft, nextBlock)

numPossibilitiesForLineUnfolded :: String -> Int
numPossibilitiesForLineUnfolded s =
    let splot = words s
        record = head splot
        combo = map (\s -> read s :: Int) (splitOn "," (last splot))
        (uRecord, uCombo) = unfold record combo in (numValid) uRecord (head uCombo) (tail uCombo)

unfold :: String -> [Int] -> (String, [Int])
unfold str combo = (concat (intersperse "?" (replicate 5 str)), concat (replicate 5 combo))

part2' lines =
    let result = sum $ map (\s -> let p = numPossibilitiesForLineUnfolded s in (trace $ show s ++ " " ++ show p) p) lines in print result

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

-- 525036306524095649 too high