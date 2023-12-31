{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Day10 where

import Utilities
import Data.List.Split
import Data.List
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

import qualified Day10_part2

-- Idea: search going from both ends of the loop and when they meet that should be the farthest point
-- Or just BFS maybe? But it's on a cycle so it might be weird. Probably makes the most sense though
-- 12:22am thought: yes this works (at least on sample and idk why it wouldn't IRL) and farthest point is the length of the loop / 2

{-
In the above diagram, you can still figure out which pipes form the main loop: they're the ones connected to S, pipes those pipes connect to, pipes those pipes connect to, and so on. Every pipe in the main loop connects to its two neighbors (including S, which will have exactly two pipes connecting to it, and which is assumed to connect back to those two pipes).
-}

{-

    | is a vertical pipe connecting north and south.
    - is a horizontal pipe connecting east and west.
    L is a 90-degree bend connecting north and east.
    J is a 90-degree bend connecting north and west.
    7 is a 90-degree bend connecting south and west.
    F is a 90-degree bend connecting south and east.
    . is ground; there is no pipe in this tile.
    S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

-}

type Coord = (Int, Int)
data Node = Node Coord [Node] deriving (Eq, Ord, Show)

data Pipe = NS | EW | NE | NW | SW | SE deriving (Eq, Ord, Show, Enum)

fromChar :: Char -> Maybe Pipe
fromChar c
    | c == '|' = Just NS
    | c == '-' = Just EW
    | c == 'L' = Just NE
    | c == 'J' = Just NW
    | c == '7' = Just SW
    | c == 'F' = Just SE
    | otherwise = Nothing

start :: Pipe -> Coord
start p
    | p == NS = (0,-1)
    | p == EW = (-1,0)
    | p == NE = (0,-1)
    | p == NW = (0,-1)
    | p == SW = (0,1)
    | p == SE = (0,1)

end :: Pipe -> Coord
end p
    | p == NS = (0,1)
    | p == EW = (1,0)
    | p == NE = (1,0)
    | p == NW = (-1,0)
    | p == SW = (-1,0)
    | p == SE = (1,0)

add :: Coord -> Coord -> Coord
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sub :: Coord -> Coord -> Coord
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

-- Function that when given a current coordinate and adjacent coordinate either goes in the pipe
-- and returns (Just newCoordinate) or Nothing.

{-
Start at a character
Check to see if the pipes around it are compatible with this pipe's direction (if the character is a pipe)

-}

charAt :: [String] -> Coord -> Char
charAt charList (x,y) = charList !! y !! x

-- all nodes should be degree 2
parseGraph :: [String] -> Set.Set Coord -> Coord -> Node
parseGraph charList visited s =
    let potentialPipes = filter (\c -> Set.notMember c visited && pipeCompatible charList s c) $ map (s `add`) [(0,1),(0,-1),(1,0),(-1,0)]
        newVisited = Set.insert s visited in
            Node s (map (parseGraph charList newVisited) potentialPipes)

inBounds :: [String] -> Coord -> Bool
inBounds charList (xc,yc) = not (xc < 0 || yc < 0 || xc >= length (head charList) || yc >= length charList)

-- If two pipes are compatible, move from start to end
pipeCompatible :: [String] -> Coord -> Coord -> Bool
pipeCompatible charList coord1 coord2 =
    if not (inBounds charList coord1 && inBounds charList coord2) then False else
    let pipe1Maybe = fromChar $ charAt charList coord1
        pipe2Maybe = fromChar $ charAt charList coord2 in
            if charAt charList coord1 == 'S' && isJust pipe2Maybe then
                let pipe2 = fromJust pipe2Maybe
                    start2 = coord2 `add` start pipe2
                    end2 = coord2 `add` end pipe2 in
                        start2 == coord1 || end2 == coord1
            else if charAt charList coord2 == 'S' && isJust pipe1Maybe then
                let pipe1 = fromJust pipe1Maybe
                    start1 = coord1 `add` start pipe1
                    end1 = coord1 `add` end pipe1 in
                        start1 == coord2 || end1 == coord2
            else not (isNothing pipe1Maybe || isNothing pipe2Maybe) && (let pipe1 = fromJust pipe1Maybe
                                                                            pipe2 = fromJust pipe2Maybe
                                                                            start1 = coord1 `add` start pipe1
                                                                            start2 = coord2 `add` start pipe2
                                                                            end1 = coord1 `add` end pipe1
                                                                            end2 = coord2 `add` end pipe2 in
                                                                                -- If the pipes are L- then end of L = start of - and start of - = end of L
                                                                                {-(trace $ "|" ++ show coord1 ++ "," ++ show coord2 ++ "| " ++ show start1 ++ " " ++ show end1 ++ " " ++ show start2 ++ " " ++ show end2 ++ "|")-} (end1 == coord2 && start2 == coord1) || (end2 == coord1 && start1 == coord2) || (start1 == coord2 && start2 == coord1) || (start2 == coord1 && start1 == coord2) || (end1 == coord2 && end2 == coord1) || (end2 == coord1 && end1 == coord2))

findStartingCoord :: [String] -> Int -> Coord
findStartingCoord strs y = if 'S' `elem` (strs !! y) then (fromJust ('S' `elemIndex` (strs !! y)), y) else findStartingCoord strs (y+1)

findLengthOfChain :: Node -> Int
findLengthOfChain (Node c []) = 0
findLengthOfChain (Node c (node:nodes)) = 1 + findLengthOfChain node

-- Part 1
part1' :: [String] -> IO ()
part1' lines =
    let start = findStartingCoord lines 0
        (Node coord nodes) = parseGraph lines Set.empty start in
            --print nodes
            print ((findLengthOfChain (head nodes) + 2) `div` 2)

part1 = do
    lines <- getLines "day10/input.txt"
    part1' lines

-- Part 2
-- Flood fill from outside and subtract # of tiles in the loop

-- https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell
replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace f i [] = []

replace2D :: (a -> a) -> (Int, Int) -> [[a]] -> [[a]]
replace2D f (x,y) = replace (replace f y) x

tilesOfLoop :: Node -> [Coord]
tilesOfLoop (Node c []) = []
tilesOfLoop (Node c (node:nodes)) = c : tilesOfLoop node

fillLoop :: Node -> [String] -> [String]
fillLoop (Node c []) strs = strs
fillLoop (Node c (node:nodes)) strs = fillLoop node (replace2D (const '-') (coordOf node `sub` c) strs)

coordOf :: Node -> Coord
coordOf (Node c _) = c

tileOutsideOfNodes :: [Coord] -> Coord
tileOutsideOfNodes nodes =
    if (0,0) `notElem` nodes then (0,0)
    else error "0,0 is in the loop :("

neighboringCoords :: [String] -> Coord -> [Coord]
neighboringCoords chars c = filter (inBounds chars) (map (c `add`) [(0,1),(0,-1),(1,0),(-1,0)])

dotTuples :: Coord -> Coord -> Int
dotTuples (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

areParallel :: Pipe -> Pipe -> Coord -> Bool
areParallel p1 p2 c = (end p1) `dotTuples` (end p2) /= 0 && (end p1) `dotTuples` c /= 0 || (start p1) `dotTuples` (end p2) /= 0 && (end p2) `dotTuples` c /= 0 || (end p1) `dotTuples` (start p2) /= 0 && (end p1) `dotTuples` c /= 0 || (start p1) `dotTuples` (start p2) /= 0 && (start p1) `dotTuples` c /= 0

perpendicular :: Coord -> Coord
perpendicular (x,y) = (y,-x)

canMove :: [String] -> [Coord] -> Coord -> Coord -> Bool
canMove chars tiles toCheck current =
    let direction = toCheck `sub` current
        tile1 = current `add` direction
        tile2 = (current `add` direction) `add` (perpendicular direction)
        pipe1 = fromChar $ charAt chars tile1
        pipe2 = fromChar $ charAt chars tile2 in
            if isJust pipe1 && isJust pipe2 then areParallel (fromJust pipe1) (fromJust pipe2) direction else False

-- Check if parallel to other neighboring ones w.r.t. 
-- Can move to all neighboring non-pipe tiles
-- (ignoring sliding between pipes for now)
bfsFill :: [String] -> [Coord] -> [Coord] -> Set.Set Coord -> Set.Set Coord
bfsFill chars tiles [] visited = visited
bfsFill chars tiles (current:coords) visited =
    let neighbors = filter (\c -> (c `notElem` tiles || canMove chars tiles c current) && c `Set.notMember` visited) (neighboringCoords chars current) in
        bfsFill chars tiles (coords ++ neighbors) (foldl (flip Set.insert) visited neighbors)

replaceChar :: String -> Int -> Char -> String
replaceChar s i c = (take i s) ++ [c] ++ (drop (i+1) s)

visualizeTileSet :: [String] -> Int -> [Coord] -> String
visualizeTileSet charList i coordList =
    if i >= length charList then ""
    else
    let currentRow = replicate (length (charList !! i)) '.'
        theseCoords = filter (\c -> snd c == i) coordList in
        foldl (\s (i,c) -> replaceChar s i c) currentRow (zip (map fst theseCoords) (replicate (length theseCoords) '#')) ++ "\n" ++ visualizeTileSet charList (i+1) coordList

part2' = Day10_part2.part2'

part2 = Day10_part2.part2

time lines =
    withArgs ["--output", "day10.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day10/input.txt"
    time lines