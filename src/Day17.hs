module Day17 where

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
import Data.Function (on)
import Text.Printf (printf)

import qualified Data.PSQueue as PSQ

-- Part 1
type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sub :: Coord -> Coord -> Coord
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

dotTuples :: Coord -> Coord -> Int
dotTuples (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

type Path = [Coord]

neighbors :: Path -> [Coord]
neighbors path =
    map ((if not (null path) then last path else (0,0)) `add`)
    (if length path < 2 then [(0,-1),(1,0),(0,1),(-1,0)] else
        let lastDir = last path `sub` (path !! (length path - 2)) in
            filter (\c -> dotTuples lastDir c == 0) [(0,-1),(1,0),(0,1),(-1,0)] ++ if length path < 4 then [lastDir] else
                let lastThree = drop (length path - 4) path
                    lastMove = ((lastThree !! 3) `sub` (lastThree !! 2))
                    penultimateMove = ((lastThree !! 2) `sub` (lastThree !! 1))
                    penpenultimateMove = ((lastThree !! 1) `sub` (head lastThree)) in
                        (if lastMove == penultimateMove && penultimateMove == penpenultimateMove then {-(trace $ "have to switch for " ++ show path)-} [] else [lastMove]))

-- Dijkstra's algorithm
-- 0. Store a Map.Map Vertex [(minCostInt, minCostPath)]
-- 1. Currently at vertex V
-- 2. Mark V as visited
-- 3. Add connections from V to V2 to the map if they are less than the minimum cost to get to V2
-- 4. Go to step (1) using the lowest cost unvisited path in the map

-- "correct" path is (0,0),(1,0),(2,0),(2,1),(3,1),(4,1),(5,1),(5,0),(6,0),(7,0),(8,0),(8,1),(8,2),(9,2),(10,2),(10,3),(10,4),(11,4),(11,5),(11,6),(11,7),(12,7),(12,8),(12,9),(12,10),(11,10),(11,11),(11,12),(12,12)
-- ok the neighbors algorithm is not messed up, it found all of these neighbors

inBounds :: [String] -> Coord -> Bool
inBounds charList (xc,yc) = not (xc < 0 || yc < 0 || xc >= length (head charList) || yc >= length charList)

charAt :: [String] -> Coord -> Char
charAt charList (x,y) = charList !! y !! x

addToPath :: [String] -> (Path, Int) -> Coord -> (Path, Int)
addToPath lines (currentPath, currentCost) newCoord = (currentPath ++ [newCoord], currentCost + (read [charAt lines newCoord] :: Int))

-- ...this is weird
-- you can't always continue from the end of the shortest path to a point
-- so e.g. the fastest to (0,3) in the example is 9 long
-- but then we can't continue downward since we have hit a train of 3 so we need to explore other ways of reaching it

-- We're analyzing the entire graph of paths (can't analyze just the graph of coordinates, because neighbors depend on the path)
-- So we have to prune somehow
-- I think we only want paths that beat the best known cost 
-- wait...[(0,0),(0,1),(0,2),(1,2),(1,1),(2,1),(2,0),(1,0),(1,1),(0,1),(0,0),(1,0),(1,1),(2,1),(3,1),(3,2),(4,2),(5,2),(6,2)] is a very dumb path (goes to (0,0) twice)
-- so we can't have that. if a neighbor is already in the path filter it out

-- only last 4 matter
trimPath :: [a] -> [a]
trimPath path = drop (max 0 (length path - 4)) path

-- Returns the cost of the minimum path
dijkstra :: [String] -> PSQ.PSQ Path Int -> Set.Set Path -> Coord -> Int
dijkstra lines frontier visited end =
    let currentMaybe = PSQ.findMin frontier in if isNothing currentMaybe then 0 {- couldn't reach end -} else
    let current = fromJust currentMaybe
        currentPath = PSQ.key current
        currentCost = PSQ.prio current
        newVisited = Set.insert (trimPath currentPath) visited in {-trace (show currentPath) $ -}if last currentPath == end then currentCost else
    let neighboringPaths = filter (\(p,i) -> trimPath p `Set.notMember` newVisited) (map (addToPath lines (currentPath, currentCost)) (filter (\neigh -> inBounds lines neigh && neigh `notElem` currentPath) (neighbors currentPath)))
        newFrontier = foldl (\q (key, prio) -> PSQ.insert key prio q) (PSQ.deleteMin frontier) neighboringPaths in
        {-trace (show neighboringPaths ++ "\n\n")-} dijkstra lines newFrontier newVisited end

part1' lines = do
    -- (0,0) to (13,12)
    let dijkstraed = dijkstra lines (PSQ.singleton [(0,0)] 0) Set.empty (12,12) in print dijkstraed
    -- [(0,0),(0,1),(1,1),(1,2),(1,3),(0,3),(0,4)]...this path is the optimal one THAT IT ISN'T FINDING

-- (1,1) -> 0.118s
-- (3,3) -> 0.118s
-- (5,5) -> 0.487s
-- (6,6) -> 3.958s
-- (7,7) -> 1m41.566s

clear :: IO ()
clear = printf "\027c"

part1 = do
    lines <- getLines "day17/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day17/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day17.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day17/input.txt"
    time lines