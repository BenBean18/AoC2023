module Day7 where

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

{-
Every hand is exactly one type. From strongest to weakest, they are:

    Five of a kind, where all five cards have the same label: AAAAA
    Four of a kind, where four cards have the same label and one card has a different label: AA8AA
    Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
    Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
    Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
    One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
    High card, where all cards' labels are distinct: 23456
-}

-- note: check 5 then 4 then 3 (they don't confirm that not > #)

fiveOfAKind :: String -> Bool
fiveOfAKind s = minimum s == maximum s

fourOfAKind :: String -> Bool
fourOfAKind s =
    let sorted = sort s in
        head sorted == (sorted !! 4) ||
        (sorted !! 1) == last sorted

allEqual :: (Eq a) => [a] -> Bool
allEqual [a,b] = a == b
allEqual (x:xs) = head xs == x && allEqual xs

fullHouse :: String -> Bool
fullHouse s =
    let sorted = sort s
        rev = reverse sorted in
        allEqual (take 2 sorted) && allEqual (drop 2 sorted) ||
        allEqual (take 2 rev) && allEqual (drop 2 rev)

threeOfAKind :: String -> Bool
threeOfAKind s =
    let sorted = sort s
        rev = reverse sorted in allEqual (take 3 sorted) || allEqual (take 3 rev)

numPairs :: String -> Int
numPairs s =
    let m = MultiSet.toMap $ foldl (flip MultiSet.insert) MultiSet.empty s in length (filter (== 2) (Map.elems m))

twoPair :: String -> Bool
twoPair s = numPairs s == 2

onePair :: String -> Bool
onePair s = numPairs s == 1

highCard :: String -> Bool
highCard = allUnique

-- higher is better
handTypeRank :: String -> Int
handTypeRank s =
    let handChecks = map (\f -> f s) [fiveOfAKind, fourOfAKind, fullHouse, threeOfAKind, twoPair, onePair, highCard]
        handChecksAndIndices = zip handChecks [0..length handChecks-1]
        conditionsSatisfied = filter fst handChecksAndIndices in
            length handChecks - snd (head conditionsSatisfied)

-- higher is better
charRanks = reverse ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

charRank :: Char -> Int
charRank c = fromJust (elemIndex c charRanks)

handRank :: String -> Int
handRank s =
    (handTypeRank s * 15 * 15 * 15 * 15 * 15) -- high enough to have more of an impact than the largest possible charRank
    + sum (zipWith (*) (map charRank s) (reverse [1..5]))

sortHands :: [String] -> [String]
sortHands = sortBy (\s1 s2 -> compare (handRank s1) (handRank s2))

part1' lines = print "Hi"

-- Part 1
part1 = do
    lines <- getLines "day6/input.txt"
    part1' lines

part2' lines =
    print "Hi"

-- Part 2
part2 = do
    lines <- getLines "day6/input.txt"
    part2' lines