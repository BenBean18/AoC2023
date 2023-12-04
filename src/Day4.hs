module Day4 where

import Utilities
import Data.List.Split

import Data.List

import Data.Text (pack, unpack, replace, isInfixOf)

import Text.Regex.Base
import Text.Regex.PCRE

import Data.Array ((!))

import qualified Data.Map as Map

import qualified Data.Set as Set

data Card = Card { i :: Int, winning :: Set.Set Int, have :: Set.Set Int } deriving (Ord, Show, Eq)

parseScratchCard :: String -> Card
parseScratchCard s =
    let [cardIdStr, cardData] = splitOn ":" s
        cardId = head $ head (cardIdStr =~ "(\\d+)")
        [winningStr, haveStr] = splitOn "|" cardData
        winningNumbers = Set.fromList $ map ((\s -> read s :: Int) . head) (winningStr =~ "(\\d+)" :: [[String]])
        haveNumbers = Set.fromList $ map ((\s -> read s :: Int) . head) (haveStr =~ "(\\d+)" :: [[String]]) in
            Card { i = read cardId :: Int, winning = winningNumbers, have = haveNumbers }

winningNumbersWeHave :: Card -> Set.Set Int
winningNumbersWeHave Card { winning = w, have = h } = Set.intersection w h

score :: Card -> Int
score c = 
    let nums = winningNumbersWeHave c in
        if Set.size nums == 0 then 0
        else 2 ^ (Set.size nums - 1)

-- Part 1
part1 = do
    lines <- getLines "day4/input.txt"
    print $ sum $ map (score . parseScratchCard) lines

-- Part 2
part2 = do
    lines <- getLines "day4/input.txt"
    print "Hi"