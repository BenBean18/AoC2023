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
import Debug.Trace
import Criterion.Main
import System.Environment

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

part1' lines = print $ sum $ map (score . parseScratchCard) lines

-- Part 1
part1 = do
    lines <- getLines "day4/input.txt"
    part1' lines

-- Memoize the number of cards won by a specific scratchcard
-- Map.Map Int Int, key = card id, value = how many
-- Also a memoization Map.Map Int [Int], key = card id, value = cards won by that card

numWinning :: Card -> Int
numWinning c = Set.size $ winningNumbersWeHave c

cardsWon :: Card -> [Int]
cardsWon c = [(i c)..(i c + numWinning c)]

evaluateCards :: [Card] -> Map.Map Int Int -> Int
evaluateCards [] cardCount = sum (Map.elems cardCount) `div` 2 -- bad hack but it works, findWithDefault 1 in both places gives 2x
evaluateCards (currentCard:cards) cardCount =
    let newCards = cardsWon currentCard
        numNewCards = Map.findWithDefault 1 (i currentCard) cardCount -- # copies of current card
        cardsToAdd = map (\cardId -> (cardId, numNewCards + Map.findWithDefault 1 cardId cardCount)) newCards
        newCardCount = foldl (\m (k, v) -> Map.insert k v m) cardCount cardsToAdd in
            evaluateCards cards newCardCount

-- Part 2
part2' lines = print $ evaluateCards (map parseScratchCard lines) Map.empty

part2 = do
    lines <- getLines "day4/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day4.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day4/input.txt"
    time lines