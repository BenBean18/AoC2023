module Day23 where

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

-- Part 1

-- We have to find the longest path with no repeated vertices (never step onto the same tile twice) -- interesting
-- Also, you have to go in the direction of a slope tile once you land on it
-- First, create a graph from the map
-- Then we can traverse it

type Coord = (Int, Int)
type Path = [Coord]

add :: Coord -> Coord -> Coord
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

-- This will be represented using adjacency lists
type Graph = Map.Map Coord [Coord]

directionOf :: Char -> Coord
directionOf '>' = (1,0)
directionOf '<' = (-1,0)
directionOf 'v' = (0,1)
directionOf '^' = (0,-1)
directionOf _ = (0,0)

charAt :: [[Char]] -> Coord -> Char
charAt charList (x,y) = charList !! y !! x

inBounds :: [[Char]] -> Coord -> Bool
inBounds charList (xc,yc) = not (xc < 0 || yc < 0 || xc >= length (head charList) || yc >= length charList)

canGo :: [[Char]] -> Coord -> Coord -> Bool
canGo chars start end =
    let dir = directionOf (charAt chars start) in
        charAt chars start /= '#' && charAt chars end /= '#' && (if dir == (0,0) then True else end == start `add` dir)

neighboringCoords :: [[Char]] -> Coord -> [Coord]
neighboringCoords chars c =
    let allDirs = filter (inBounds chars) (map (c `add`) [(0,1),(0,-1),(1,0),(-1,0)]) in filter (canGo chars c) allDirs

parseGraph :: [[Char]] -> Graph
parseGraph chars =
    let allCoords = concatMap (\x -> map (\y -> (x,y)) [0..length chars-1]) [0..length (head chars)-1] in
        foldl (\currentMap coord -> Map.insert coord (neighboringCoords chars coord) currentMap) Map.empty allCoords

-- so can we use dijkstra's algorithm with a max heap instead of a min heap?
-- but that's basically just a DFS
-- "This hike contains 94 steps. (The other possible hikes you could have taken were 90, 86, 82, 82, and 74 steps long.)"

-- does a depth-first search to find all possible paths to the end
dfs :: Graph -> Path -> Coord -> [Path]
dfs graph currentPath destination =
    let lastCoord = last currentPath
        neighbors = filter (`notElem` currentPath) (graph Map.! lastCoord)
        neighborPaths = map (\c -> currentPath ++ [c]) neighbors in
            (if (last currentPath) == destination then [currentPath] else []) ++
            concatMap (\p -> dfs graph p destination) neighborPaths

part1' lines =
    let graph = parseGraph lines
        start = (fromJust $ '.' `elemIndex` head lines, 0)
        end = (fromJust $ '.' `elemIndex` last lines, length lines - 1)
        paths = dfs graph [start] end
        steps = map (\p -> length p - 1) paths
        maxSteps = maximum steps in
        print maxSteps

part1 = do
    lines <- getLines "day23/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day23/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day23.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day23/input.txt"
    time lines