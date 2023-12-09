module Day6 where

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

-- it's accelerating for n milliseconds but not moving, then just moving forward at a specific velocity
-- so we just calculate final velocity

-- https://www.desmos.com/calculator/lvvmk043dk

data Race = Race { time :: Int, distance :: Int } deriving (Ord, Show, Eq)

parseInts :: String -> [Int]
parseInts str = map (\s -> read s :: Int) (tail $ wordsBy (== ' ') str)

intsToRace :: (Int, Int) -> Race
intsToRace (time, distance) = Race { time = time, distance = distance }

parseRaces :: [String] -> [Race]
parseRaces [times, distances] = zipWith (curry intsToRace) (parseInts times) (parseInts distances)

minTime :: Race -> Int
minTime Race { time = tInt, distance = dInt } =
    let t = fromIntegral tInt :: Double
        d = fromIntegral dInt :: Double in floor ((-t + sqrt ((t ** 2) - (4 * d))) / (-2))

maxTime :: Race -> Int
maxTime Race { time = tInt, distance = dInt } =
    let t = fromIntegral tInt :: Double
        d = fromIntegral dInt :: Double in floor ((-t - sqrt ((t ** 2) - (4 * d))) / (-2))

numberOfWays :: Race -> Int
numberOfWays race = maxTime race - minTime race

part1' lines = 
    let result = product $ map numberOfWays (parseRaces lines) in print result

-- Part 1
part1 = do
    lines <- getLines "day6/input.txt"
    part1' lines

-- Part 2 is part 1 but you do s/(\d) + (\d)/$1$2

onlyNumber :: String -> String
onlyNumber s = ": " ++ concatMap head (s =~ "(\\d+)" :: [[String]])

part2' lines =
    let result = product $ map numberOfWays (parseRaces (map onlyNumber lines)) in print result

-- Part 2
part2 = do
    lines <- getLines "day6/input.txt"
    part2' lines

benchTime lines =
    withArgs ["--output", "day6.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day6/input.txt"
    benchTime lines