module Day23_part2 where

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
import qualified Data.PSQueue as PSQ

-- Part 2

-- Same as part 1, except slope tiles are just regular tiles
-- hmmm... from Wikipedia: "However, [the longest path problem] has a linear time solution for directed acyclic graphs, which has important applications in finding the critical path in scheduling problems. "

{-
THIS MAKES SO MUCH SENSE
A longest path between two given vertices s and t in a weighted graph G is the same thing as a shortest path in a graph −G derived from G by changing every weight to its negation. Therefore, if shortest paths can be found in −G, then longest paths can also be found in G.
-}

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
        charAt chars start /= '#' && charAt chars end /= '#'-- && (if dir == (0,0) then True else end == start `add` dir)

neighboringCoords :: [[Char]] -> Coord -> [Coord]
neighboringCoords chars c =
    let allDirs = filter (inBounds chars) (map (c `add`) [(0,1),(0,-1),(1,0),(-1,0)]) in filter (canGo chars c) allDirs

parseGraph :: [[Char]] -> Graph
parseGraph chars =
    let allCoords = filter (\c -> charAt chars c /= '#') $ concatMap (\x -> map (\y -> (x,y)) [0..length chars-1]) [0..length (head chars)-1] in
        foldl (\currentMap coord -> Map.insert coord (neighboringCoords chars coord) currentMap) Map.empty allCoords

dfs :: Graph -> Set.Set Coord -> (Path,Int) -> Coord -> Coord -> [Int]
dfs graph visited (currentPath,currentLen) currentCoord destination = (trace $ show currentPath ++ " " ++ show currentLen) $
    let lastCoord = currentCoord
        neighbors = filter (\c -> c /= currentCoord && c `Set.notMember` visited) (graph Map.! lastCoord)
        newVisited = Set.insert currentCoord visited
        lowDegreeNeighbors = filter (\coord -> length (graph Map.! coord) <= 2) neighbors
        highDegreeNeighbors = filter (\coord -> length (graph Map.! coord) > 2) neighbors -- only add degree 2+ vertices to path
        neighborPaths = map (\c -> currentPath ++ [c]) highDegreeNeighbors in
            (if currentCoord == destination then {-(trace "found dest path")-} [currentLen] else []) ++
            concatMap (\n -> dfs graph newVisited (currentPath,currentLen+1) n destination) lowDegreeNeighbors ++ concatMap (\p -> dfs graph newVisited (p,currentLen+1) (last p) destination) neighborPaths

-- 4906 too low :(
-- 4906 continues to be too low, even though both Dijkstra's and SPFA find it
-- ...........................
-- 4279 too low...i am silly idk why i thought this was higher

part2' lines =
    let graph = parseGraph lines
        start = (fromJust $ '.' `elemIndex` head lines, 0)
        end = (fromJust $ '.' `elemIndex` last lines, length lines - 1)
        paths = dfs graph Set.empty ([start],0) start end
        steps = {-map (\p -> length p - 1) -}paths
        maxSteps = maximum steps in
        print maxSteps

part2 = do
    lines <- getLines "day23/input.txt"
    part2' lines