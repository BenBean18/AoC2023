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

dfs :: Graph -> Set.Set Coord -> Int -> Coord -> Coord -> [Int]
dfs graph visited currentLen currentCoord destination = --(trace $ show currentLen) $
    let lastCoord = currentCoord
        neighbors = filter (\c -> c /= currentCoord && c `Set.notMember` visited) (graph Map.! lastCoord)
        newVisited = Set.insert currentCoord visited
        lowDegreeNeighbors = filter (\coord -> length (graph Map.! coord) <= 2) neighbors
        highDegreeNeighbors = filter (\coord -> length (graph Map.! coord) > 2) neighbors in -- only add degree 2+ vertices to path
            (if currentCoord == destination then (trace $ show currentLen) [currentLen] else []) ++
            concatMap (\c -> dfs graph newVisited (currentLen+1) c destination) neighbors

pathNeighbors :: Graph -> (Path,Set.Set Coord,Int) -> [(Path,Set.Set Coord,Int)]
pathNeighbors graph (path,visited,len) =
    let currentCoord = last path
        currentPath = init path
        neighbors = filter (\c -> c /= currentCoord && c `Set.notMember` visited) (graph Map.! currentCoord)
        newVisited = Set.insert currentCoord visited
        lowDegreeNeighbors = filter (\coord -> length (graph Map.! coord) <= 2) neighbors
        highDegreeNeighbors = filter (\coord -> length (graph Map.! coord) > 2) neighbors
        neighborPaths = map (\c -> (path ++ [c],newVisited,len-1)) highDegreeNeighbors
                     ++ map (\c -> (currentPath ++ [c],newVisited,len-1)) lowDegreeNeighbors in neighborPaths

-- cost map should be the minimum cost to get to path, as well as our visited set
dijkstra :: Graph -> PSQ.PSQ Path (Int, Set.Set Coord) -> Set.Set Path -> Coord -> Int
dijkstra graph priorityQueue visited endingCoord =
    let Just (nextBinding, newPQ) = PSQ.minView priorityQueue
        currentPath = PSQ.key nextBinding
        currentCoord = last currentPath
        (currentCost, currentVisited) = PSQ.prio nextBinding in
    if currentCoord == endingCoord then -currentCost
    else if currentPath `Set.member` visited then (trace "already visited") dijkstra graph newPQ visited endingCoord
    else
    let newVisited = Set.insert currentPath visited
        neighbors = pathNeighbors graph (currentPath, currentVisited, currentCost)
        neighborsToInsert = map (\(p,v,c) -> (p, (c, v))) neighbors
        newPQWithNeighbors = foldl (\q (key, prio) -> PSQ.insert key prio q) newPQ neighborsToInsert in dijkstra graph newPQWithNeighbors newVisited endingCoord

-- 4906 too low :(
-- 4906 continues to be too low, even though both Dijkstra's and SPFA find it
-- ...........................
-- 4279 too low...i am silly idk why i thought this was higher

-- so... the graph of paths *is* a directed acyclic graph
-- can only go from one path to the next and can't go "back"

-- create the graph of paths (compressing to only include start, decision points, and end ONLY if end is the ending point)
-- this is a tree, so it's a DAG
-- should have about 2^34 paths = ~17 billion = doable
-- also at least 477000 paths exist that reach the end

-- also just brute forcing every single path and tracing once reaches destination
-- solution MUST be at least 6478 (edit: actually 6582)
-- just found 6734 on the 443594th path

-- could run Dijkstra's on the tree of paths...i think?

-- idea: find the *shortest* path through every pair of decision points
-- then you have a fully connected graph and can relatively easily run BFS/DFS

type GraphWithCost = Map.Map Coord [(Coord,Int,Set.Set Coord)]

-- https://stackoverflow.com/questions/34044366/how-to-extract-all-unique-pairs-of-a-list-in-haskell
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

findJunctions :: [[Char]] -> [Coord]
findJunctions chars = 
    let allCoords = filter (\c -> charAt chars c /= '#') $ concatMap (\x -> map (\y -> (x,y)) [0..length chars-1]) [0..length (head chars)-1]
        junctions = filter (\c -> length (neighboringCoords chars c) > 2) allCoords in junctions

dijkstra' :: Graph -> PSQ.PSQ Coord (Int, Set.Set Coord) -> Set.Set Coord -> Coord -> (Int, Set.Set Coord)
dijkstra' graph priorityQueue visited endingCoord =
    let Just (nextBinding, newPQ) = PSQ.minView priorityQueue
        currentCoord = PSQ.key nextBinding
        (currentCost, currentPath) = PSQ.prio nextBinding in
    if currentCoord == endingCoord then (currentCost, visited)
    else if currentCoord `Set.member` visited then (trace "already visited") dijkstra' graph newPQ visited endingCoord
    else
    let newVisited = Set.insert currentCoord visited
        neighbors = filter (`Set.notMember` currentPath) (graph Map.! currentCoord)
        -- now actually trying to find shortest path
        neighborsToInsert = map (\n -> (n, (currentCost + 1, Set.insert n currentPath))) neighbors
        newPQWithNeighbors = foldl (\q (key, prio) -> PSQ.insert key prio q) newPQ neighborsToInsert in dijkstra' graph newPQWithNeighbors newVisited endingCoord

parseSparseGraph :: [[Char]] -> [Coord] -> GraphWithCost
parseSparseGraph chars additionalCoords =
    let junctions = findJunctions chars ++ additionalCoords
        junctionPairs = pairs junctions
        dumbGraph = parseGraph chars
        distances = map (\(c1,c2) -> ((c1,c2),(dijkstra' dumbGraph (PSQ.singleton c1 (0, Set.empty)) Set.empty c2))) junctionPairs
        newGraph = foldl (\m ((c1,c2),(cost,v)) -> Map.insertWith (++) c2 [(c1,cost,(Set.delete c2 (Set.delete c1 v)))] (Map.insertWith (++) c1 [(c2,cost,(Set.delete c2 (Set.delete c1 v)))] m)) Map.empty distances in newGraph

dfs' :: GraphWithCost -> Map.Map Coord Int -> Int -> Coord -> Coord -> [Int]
dfs' graph visited currentLen currentCoord destination = --(trace $ show currentLen) $
    let lastCoord = currentCoord
        neighbors = filter (\(c,cost,v) -> c /= currentCoord{- && c `Map.notMember` visited-}) (graph Map.! lastCoord)
        newVisited = Map.insertWith (+) currentCoord 1 visited in
            (if currentCoord == destination && Map.size (Map.filter (> 3) newVisited) == 0 then (trace $ show currentLen ++ " " ++ show (Map.elems newVisited) ++ "\n") [currentLen] else []) ++
            concatMap (\(coord,cost,v) -> dfs' graph (Map.unionWith (+) newVisited (Map.fromList (map (\c -> (c,1)) (Set.toList v)))) (currentLen+cost) coord destination) neighbors

part2' lines =
    let graph = parseGraph lines
        start = (fromJust $ '.' `elemIndex` head lines, 0)
        end = (fromJust $ '.' `elemIndex` last lines, length lines - 1)
        -- paths = dfs graph Set.empty 0 start end
        -- steps = {-map (\p -> length p - 1) -}paths
        -- maxSteps = maximum steps
        maxStepsDijkstra = dijkstra graph (PSQ.singleton [start] (0, Set.empty)) Set.empty end
        junctions = findJunctions lines
        g2 = parseSparseGraph lines [start,end]
        paths = dfs' g2 Map.empty 0 start end in do
        -- print maxStepsDijkstra
        -- print maxSteps
        -- note: junctions are [(9,15),(11,57),(15,33),(15,101),(17,77),(29,103),(31,5),(35,67),(37,37),(41,77),(41,133),(53,55),(55,109),(57,125),(59,89),(61,7),(67,43),(77,7),(77,39),(77,123),(79,63),(79,101),(83,81),(103,19),(105,85),(107,33),(107,133),(109,61),(111,107),(123,59),(123,107),(123,127),(125,81),(135,31)]
        -- print g2
        print paths

part2 = do
    lines <- getLines "day23/input.txt"
    part2' lines