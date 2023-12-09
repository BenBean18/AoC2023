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
    let matches = map head (s =~ "([A-Z]+)" :: [[String]]) in
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
part2' lines = print "Hi"

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