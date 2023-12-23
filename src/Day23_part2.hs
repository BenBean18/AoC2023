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

charAt :: [[Char]] -> Coord -> Char
charAt charList (x,y) = charList !! y !! x

inBounds :: [[Char]] -> Coord -> Bool
inBounds charList (xc,yc) = not (xc < 0 || yc < 0 || xc >= length (head charList) || yc >= length charList)

canGo :: [[Char]] -> Coord -> Coord -> Bool
canGo chars start end = charAt chars start /= '#' && charAt chars end /= '#'

neighboringCoords :: [[Char]] -> Coord -> [Coord]
neighboringCoords chars c =
    let allDirs = filter (inBounds chars) (map (c `add`) [(0,1),(0,-1),(1,0),(-1,0)]) in filter (canGo chars c) allDirs

parseGraph :: [[Char]] -> Graph
parseGraph chars =
    let allCoords = filter (\c -> charAt chars c /= '#') $ concatMap (\x -> map (\y -> (x,y)) [0..length chars-1]) [0..length (head chars)-1] in
        foldl (\currentMap coord -> Map.insert coord (neighboringCoords chars coord) currentMap) Map.empty allCoords

-- so apparently on a directed acyclic graph if you do a topological sort (every vertex comes before the vertices depending on it): https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search,
-- shortest path is O(n+m)
-- is our graph acyclic? no... :(
-- and it's not directed either!

-- but Dijkstra's with negative weights will still be better than a DFS over the entire graph
-- summary:
-- have a Map.Map vertex (current min cost to vertex, set of vertices used to get there) initialized to (inf, Set.empty)
-- actually, we can represent this as a priority queue to use min-heap property
-- while not all nodes are visited:
-- pick a current node (lowest cost in the map, use a min heap) and store current vertex, cost, and set of vertices used to get there
-- for all connected edges, if they have a lesser cost than the current minimum to get to the next vertex, replace in the map

-- having (Int, Set.Set Coord) for comparison is fine since tuples are compared from first to last element
dijkstra :: Graph -> PSQ.PSQ Coord (Int, Set.Set Coord) -> Set.Set Coord -> Coord -> Int
dijkstra graph priorityQueue visited endingCoord =
    let Just (nextBinding, newPQ) = PSQ.minView priorityQueue
        currentCoord = PSQ.key nextBinding
        (currentCost, currentPath) = PSQ.prio nextBinding in
    if currentCoord == endingCoord then -currentCost
    else if currentCoord `Set.member` visited then (trace "already visited") dijkstra graph newPQ visited endingCoord
    else
    let newVisited = Set.insert currentCoord visited
        neighbors = filter (`Set.notMember` currentPath) (graph Map.! currentCoord)
        -- note: every edge has cost -1 since we want to find the longest path (aka shortest path with negative weights)
        neighborsToInsert = map (\n -> (n, (currentCost - 1, Set.insert n currentPath))) neighbors
        newPQWithNeighbors = foldl (\q (key, prio) -> PSQ.insert key prio q) newPQ neighborsToInsert in dijkstra graph newPQWithNeighbors newVisited endingCoord
-- ...oops so apparently dijkstra's fails with negative weighted graphs? https://stackoverflow.com/questions/6799172/negative-weights-using-dijkstras-algorithm/6799344#6799344
{-
The Bellman–Ford algorithm is an algorithm that computes shortest paths from a single source vertex to all of the other vertices in a weighted digraph.[1] It is slower than Dijkstra's algorithm for the same problem, but more versatile, as it is capable of handling graphs in which some of the edge weights are negative numbers.

from what i can see, the Bellman-Ford algorithm is like Dijkstra's but instead of only checking the edges from the closest unvisited vertex, it checks all the edges
and it does this (# of vertices) - 1 times... need to read more
-}

-- oh this is clever: https://en.wikipedia.org/wiki/Shortest_path_faster_algorithm
-- you only want to check a vertex again if you found a better path to it in the previous iteration (no benefit to doing so otherwise)

bellmanFord :: Graph -> Map.Map Coord (Int, Set.Set Coord) -> Coord -> Int -> Int
bellmanFord graph costMap endingCoord 0 = fst $ costMap Map.! endingCoord
bellmanFord graph costMap endingCoord i =
    -- relax all edges from all vertices
    let neighbors = concatMap (\(coord, (cost, path)) ->
            map (\c -> 
                    (c, (cost - 1, Set.insert c path))
                )
                (filter (`Set.notMember` path) (graph Map.! coord))
                                                                    ) (Map.toList costMap)
        newCostMap = foldl (\currentMap (key, value) -> if fst (currentMap Map.! key) > fst value then Map.insert key value currentMap else currentMap) costMap neighbors in bellmanFord graph newCostMap endingCoord (i-1)

spfa :: Graph -> Map.Map Coord (Int, Set.Set Coord) -> Set.Set Coord -> Coord -> Int
spfa graph costMap candidates endingCoord = if Set.null candidates then fst $ costMap Map.! endingCoord else
    -- relax all edges from all vertices
    let neighbors = concatMap (\coord -> map (\c -> (c, ((fst (costMap Map.! coord)) - 1, Set.insert c (snd (costMap Map.! coord))))) (filter (`Set.notMember` (snd (costMap Map.! coord))) (graph Map.! coord))) candidates
        newCostMap = foldl (\currentMap (key, value) -> if fst (currentMap Map.! key) > fst value then Map.insert key value currentMap else currentMap) costMap neighbors in (trace $ show $ length candidates) spfa graph newCostMap (Set.fromList (map fst neighbors)) endingCoord

-- 4906 too low :(
-- 4906 continues to be too low, even though both Dijkstra's and SPFA find it
-- ...........................
-- 4279 too low...i am silly idk why i thought this was higher

-- ok new idea
-- what if instead of having a graph of coordinates, we have a graph of paths
-- then that's a directed acyclic graph
-- but the search space is HUGE

-- ...something's up
-- this graph should be undirected, because there are no slopes
-- so the path from start to end should equal the path from end to start
-- but it doesn't

-- and interestingly, using Dijkstra's, the path from end to start is 2278 long which is the part 1 answer

-- THIS IS AN UNDIRECTED GRAPH!

-- so something with comparison was messed up, fst (currentMap Map.! key) > fst value /= currentMap Map.! key > value
-- wait this is because the path *matters* when choosing which path to go to next (if we have already visited somewhere that makes a difference)
-- do we have to try every possible path
-- again, maybe try a graph of paths
-- CHECK THE CRUCIBLE PROBLEM for an example of something similar!
-- have to try comething different
-- anyway, it returns 5250 with new comparison so let's try that -- assuming it will be incorrect

{-
That's not the right answer. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Because you have guessed incorrectly 4 times on this puzzle, please wait 5 minutes before trying again. [Return to Day 23]
-}

-- To verify if I have a correct algorithm: confirm start -> end == end -> start on sample data

part2' lines =
    let graph = parseGraph lines
        start = (fromJust $ '.' `elemIndex` head lines, 0)
        end = (fromJust $ '.' `elemIndex` last lines, length lines - 1)
        longestPath = spfa graph (Map.mapWithKey (\coord _ -> (0, Set.singleton coord)) graph) (Set.singleton start) end
        -- longestPath = bellmanFord graph (PSQ.singleton end (0, Set.singleton end)) Set.empty start
        -- verify every child of every coordinate has a child that is the parent
        undirected = and (concatMap (\coord -> (map (\c -> coord `elem` (graph Map.! c)) (graph Map.! coord))) (Map.keys graph)) in do
        print undirected
        print longestPath

part2 = do
    lines <- getLines "day23/input.txt"
    part2' lines