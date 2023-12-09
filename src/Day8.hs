module Day8 where

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

-- name, left, right
data Node = Node { name :: String, left :: String, right :: String } deriving (Ord, Eq, Show)
type DesertMap = Map.Map String Node
type Instructions = String

go :: DesertMap -> Node -> Char -> Node
go m n 'L' = m Map.! left n
go m n 'R' = m Map.! right n

parseNode :: String -> Node
parseNode s =
    let matches = map head (s =~ "([A-Z0-9]+)" :: [[String]]) in
        Node { name = head matches, left = matches !! 1, right = matches !! 2 }

parseMap :: [String] -> DesertMap
parseMap strs =
    let nodes = map parseNode strs in
        Map.fromList (map (\n -> (name n, n)) nodes)

parse :: [String] -> (DesertMap, Instructions)
parse strs = (parseMap (drop 2 strs), cycle (head strs))

findZZZ' :: DesertMap -> Instructions -> Node -> Int
findZZZ' _ _ Node { name = "ZZZ" } = 0
findZZZ' m (i:is) n = 1 + findZZZ' m is (go m n i)

part1' :: [String] -> IO ()
part1' lines =
    let (map, instructions) = parse lines
        result = findZZZ' map instructions (map Map.! "AAA") in print result

part1 = do
    lines <- getLines "day8/input.txt"
    part1' lines

-- Part 2
doAll :: DesertMap -> Instructions -> [Node] -> Int
doAll m (i:is) ns = (trace $ show (map name ns)) (
    if all (\n -> last (name n) == 'Z') ns then 0
    else 1 + doAll m is (map (\n -> go m n i) ns))

-- Looks like in the inputs, there is a cycle here
-- If it takes n steps to reach a finishing point, it will take n more steps to do it again
-- So we just find the time to finish and that's a cycle with length n
-- Then least common multiple of all of the cycles to find when they all hit an ending point
findTimeToFinish :: DesertMap -> Instructions -> Node -> Int
findTimeToFinish m (i:is) n =
    if last (name n) == 'Z' then 0
    else 1 + findTimeToFinish m is (go m n i)

lcmList :: [Int] -> Int
lcmList = foldl lcm 1

toString :: [Char] -> String
toString c = c

part2' lines =
    let (desertMap, instructions) = parse lines
        startingNames = filter (\s -> last s == 'A') (Map.keys desertMap)
        startingNodes = map (desertMap Map.!) startingNames
        result = lcmList $ map (findTimeToFinish desertMap instructions) startingNodes in print result

part2 = do
    lines <- getLines "day8/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day8.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day8/input.txt"
    time lines