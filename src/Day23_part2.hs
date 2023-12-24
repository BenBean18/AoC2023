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
-- CHECK THE CRUCIBLE PROBLEM for an example of something similar! (day 17)
-- have to try comething different
-- anyway, it returns 5250 with new comparison so let's try that -- assuming it will be incorrect

{-
That's not the right answer. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Because you have guessed incorrectly 4 times on this puzzle, please wait 5 minutes before trying again. [Return to Day 23]
-}

-- To verify if I have a correct algorithm: confirm start -> end == end -> start on sample data

-- Find *only* the decision points (degree >2 vertices), there aren't that many
-- Store the cost to move from one vertex to the next decision point
-- Start at TL 

type GraphWithCost = Map.Map Coord [(Coord,Int)]

parseGraphCost :: [[Char]] -> GraphWithCost -> Set.Set Coord -> [Coord] -> Coord -> GraphWithCost
parseGraphCost chars currentGraph visited coordsVisited currentCoord =
    if Map.size currentGraph >= 33 then (trace $ show currentGraph) currentGraph else
    let neighbors = filter (\c -> c `notElem` coordsVisited && isNothing (Map.lookup c currentGraph) && c `Set.notMember` visited) (neighboringCoords chars currentCoord)
        newVisited = Set.insert currentCoord visited in
        if length neighbors == 0 then currentGraph
        else if length neighbors == 1 then parseGraphCost chars currentGraph newVisited (coordsVisited ++ neighbors) (head neighbors)
        else 
            let newGraph = Map.unions $ map (\n -> (Map.insert (head coordsVisited) [(n, (length coordsVisited))] (Map.insert n [(head coordsVisited, (length coordsVisited))] currentGraph))) neighbors in
            Map.unions $ map (\n -> parseGraphCost chars newGraph (foldl (flip Set.insert) newVisited neighbors) [n] n) neighbors

spfa' :: GraphWithCost -> Map.Map Coord (Int, Set.Set Coord) -> Set.Set Coord -> Coord -> Int
spfa' graph costMap candidates endingCoord = if Set.null candidates then fst $ costMap Map.! endingCoord else
    -- relax all edges from all vertices
    let neighbors = concatMap (\coord -> map (\(c,cost) -> (c, ((fst (costMap Map.! coord)) - cost, Set.insert c (snd (costMap Map.! coord))))) (filter (\(c,cost) -> c `Set.notMember` (snd (costMap Map.! coord))) (graph Map.! coord))) candidates
        newCostMap = foldl (\currentMap (key, value) -> if fst (currentMap Map.! key) > fst value then Map.insert key value currentMap else currentMap) costMap neighbors in (trace $ show $ length candidates) spfa' graph newCostMap (Set.fromList (map fst neighbors)) endingCoord

part2' lines =
    let graph = parseGraph lines
        start = (fromJust $ '.' `elemIndex` head lines, 0)
        end = (fromJust $ '.' `elemIndex` last lines, length lines - 1)
        theMap = Map.fromList [((1,0),[((9,16),72)]),((9,16),[((15,34),176)]),((10,15),[((1,0),72)]),((11,56),[((17,76),174)]),((11,58),[((17,78),174)]),((12,57),[((15,34),172)]),((15,32),[((11,56),172)]),((15,34),[((11,58),172)]),((15,100),[((40,133),510)]),((15,102),[((41,132),510)]),((16,33),[((9,16),176)]),((16,101),[((17,78),290)]),((17,76),[((15,100),290)]),((17,78),[((15,102),290)]),((18,77),[((11,58),174)]),((28,103),[((41,132),170)]),((29,102),[((41,76),142)]),((29,104),[((41,78),142)]),((30,5),[((37,36),258)]),((30,103),[((41,132),170)]),((31,6),[((60,7),156)]),((32,5),[((61,8),156)]),((34,67),[((41,76),36)]),((35,66),[((37,36),96)]),((35,68),[((37,38),96)]),((36,37),[((35,66),96)]),((36,67),[((41,76),36)]),((37,36),[((32,5),258)]),((37,38),[((31,6),258)]),((38,37),[((35,66),96)]),((40,77),[((29,102),142)]),((40,133),[((29,104),170)]),((41,76),[((35,66),36)]),((41,78),[((58,89),158)]),((41,132),[((29,102),170)]),((42,77),[((29,102),142)]),((42,133),[((15,102),510)]),((52,55),[((67,44),82)]),((53,54),[((36,67),130)]),((53,56),[((67,44),82)]),((54,55),[((67,44),82)]),((54,109),[((59,90),112)]),((55,108),[((57,124),94)]),((55,110),[((58,125),94)]),((56,109),[((59,90),112)]),((56,125),[((55,110),94)]),((57,124),[((76,123),182)]),((58,89),[((53,56),124)]),((58,125),[((55,110),94)]),((59,88),[((82,81),152)]),((59,90),[((53,56),124)]),((60,7),[((67,42),238)]),((60,89),[((53,56),124)]),((61,8),[((67,44),238)]),((62,7),[((32,5),156)]),((66,43),[((61,8),238)]),((67,42),[((38,37),188)]),((67,44),[((53,56),82)]),((68,43),[((61,8),238)]),((76,7),[((77,38),132)]),((76,39),[((77,8),132)]),((76,123),[((79,102),160)]),((77,8),[((77,40),132)]),((77,38),[((68,43),34)]),((77,40),[((68,43),34)]),((77,122),[((79,100),160)]),((78,7),[((62,7),72)]),((78,39),[((68,43),34)]),((78,63),[((77,40),142)]),((78,101),[((83,82),92)]),((78,123),[((58,125),182)]),((79,62),[((54,55),142)]),((79,64),[((54,55),142)]),((79,100),[((56,109),128)]),((79,102),[((56,109),128)]),((80,63),[((54,55),142)]),((80,101),[((56,109),128)]),((82,81),[((79,64),102)]),((83,80),[((60,89),152)]),((83,82),[((79,102),92)]),((84,81),[((60,89),152)]),((102,19),[((135,30),424)]),((103,20),[((78,7),214)]),((104,19),[((78,7),214)]),((104,85),[((109,62),120)]),((105,84),[((84,81),150)]),((105,86),[((84,81),150)]),((106,33),[((109,60),214)]),((106,85),[((84,81),150)]),((106,133),[((111,108),154)]),((107,32),[((78,39),268)]),((107,34),[((78,39),268)]),((107,132),[((111,106),154)]),((108,33),[((78,39),268)]),((108,61),[((105,84),120)]),((108,133),[((78,123),232)]),((109,60),[((80,63),124)]),((109,62),[((105,86),120)]),((110,61),[((80,63),124)]),((110,107),[((107,132),154)]),((111,106),[((80,101),206)]),((111,108),[((80,101),206)]),((112,107),[((80,101),206)]),((122,59),[((125,80),260)]),((122,107),[((125,82),272)]),((122,127),[((123,108),112)]),((123,58),[((110,61),28)]),((123,60),[((110,61),28)]),((123,106),[((112,107),28)]),((123,108),[((112,107),28)]),((123,126),[((108,133),118)]),((123,128),[((108,133),118)]),((124,81),[((123,106),272)]),((125,80),[((106,85),84)]),((125,82),[((123,108),272)]),((134,31),[((123,58),256)]),((135,30),[((123,58),256)]),((135,32),[((108,33),182)])]
        longestPath = spfa' theMap (Map.mapWithKey (\coord _ -> (0, Set.singleton coord)) graph) (Set.singleton start) (123,108)
        -- longestPath = bellmanFord graph (PSQ.singleton end (0, Set.singleton end)) Set.empty start
        -- verify every child of every coordinate has a child that is the parent
        undirected = and (concatMap (\coord -> (map (\c -> coord `elem` (graph Map.! c)) (graph Map.! coord))) (Map.keys graph)) in do
        print start
        print end
        print (Map.size (Map.filter (\l -> length l > 2) graph))
        print longestPath
        -- print $ parseGraphCost lines Map.empty (Set.singleton start) [start] start
        -- print undirected
        -- print longestPath

part2 = do
    lines <- getLines "day23/input.txt"
    part2' lines