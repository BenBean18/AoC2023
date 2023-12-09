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
        head sorted == (sorted !! 3) ||
        (sorted !! 1) == last sorted

allEqual :: (Eq a) => [a] -> Bool
allEqual [x] = True
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
        rev = reverse sorted in allEqual (take 3 sorted) || allEqual (take 3 rev) || allEqual (drop 1 (take 4 sorted))

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
        conditionsSatisfied = filter fst handChecksAndIndices in {-(trace $ show s ++ " " ++ show handChecks)-} (
            (length handChecks + 1) - snd (head conditionsSatisfied))

-- higher is better
charRanks = reverse ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

charRank :: Char -> Int
charRank c = fromJust (elemIndex c charRanks) + 1

handRank :: String -> Int
handRank s =
    (handTypeRank s * 15 * 15 * 15 * 15 * 15 * 15 * 15) -- high enough to have more of an impact than the largest possible charRank
    + sum (zipWith (*) (map charRank s) (map (15 ^) (reverse [1..5])))

sortHands :: [String] -> [String]
sortHands = sortBy (\s1 s2 -> compare (handRank s1) (handRank s2))

getScores :: [(String, Int)] -> [Int]
getScores handsAndBids =
    let ranks = map (\(h,b) -> handRank h) handsAndBids
        bigRanksAndBids = map (\((h,b),r) -> (r,b)) (sortBy (\s1 s2 -> compare (snd s1) (snd s2)) (zip handsAndBids ranks))
        ranksAndBids = zip [1..length handsAndBids] (map snd bigRanksAndBids) in {-(trace $ show ranksAndBids)-} (map (uncurry (*)) ranksAndBids)

{-

getScores :: [(String, Int)] -> [Int]
getScores handsAndBids =
    let ranks = map (\(h,b) -> handRank h) handsAndBids
        sorted = sortBy  handsAndBids
        ranksAndBids = zipWith (curry (\((hand,bid),rank) -> (rank, bid))) sorted [1..length sorted] in {-(trace $ show ranksAndBids)-} (map (uncurry (*)) ranksAndBids)

-}

testData :: [(String, Int)]
testData = [("32T3K",765), ("KK677",28), ("KTJJT",220), ("QQQJA",483), ("T55J5",684)]

parseHand :: String -> (String, Int)
parseHand s =
    let w = words s in (head w, read (w !! 1) :: Int)

parseHands :: [String] -> [(String, Int)]
parseHands = map parseHand

part1' lines =
    let hands = parseHands lines
        result = sum $ getScores hands in print result

-- 252936384 too low
-- 253687535 too high

-- Part 1
part1 = do
    lines <- getLines "day7/input.txt"
    part1' lines

part2' lines =
    print "Hi"

-- Part 2
part2 = do
    lines <- getLines "day7/input.txt"
    part2' lines