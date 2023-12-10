module Day10 where

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

-- usePipe :: [String] -> Coord -> Coord -> Maybe Coord
-- usePipe charList (xc, yc) (xp, yp) =
--     -- bounds checking
--     if xc < 0 || yc < 0 || xc >= length (head charList) || yc > length charList || xp < 0 || yp < 0 || xp >= length (head charList) || yp > length charList || isNothing (fromChar (charList !! yp !! xp)) then Nothing
--     else
--         let sp = fromChar (charList !! yc !! xc)
--             p = fromJust $ fromChar (charList !! yp !! xp)
--             s = (xp,yp) `add` start p
--             e = (xp,yp) `add` end p in (trace $ show sp ++ " " ++ show p)
--             (if isJust sp && not (((xc,yc) `add` start (fromJust sp) == (xp,yp) || (xc,yc) `add` end (fromJust sp) == (xp,yp)) && (
--             (xc,yc) `add` start (fromJust sp) == (xp,yp) `sub` start p || (xc,yc) `add` end (fromJust sp) == (xp,yp) `sub` start p || (xc,yc) `add` start (fromJust sp) == (xp,yp) `sub` end p || (xc,yc) `add` end (fromJust sp) == (xp,yp) `sub` end p)) then Nothing
--             else if s == (xc,yc) then Just (xp,yp) -- Just e
--             else if e == (xc,yc) then Just (xp,yp) -- Just s
--             else Nothing)

-- checkPipes :: [String] -> Coord -> [Coord]
-- checkPipes charList s =
--     mapMaybe (usePipe charList s . (s `add`)) [(0,1),(0,-1),(1,0),(-1,0)] -- haskell language server is great

-- followPipe :: [String] -> Set.Set Coord -> Coord -> [Coord]
-- followPipe charList visited current = filter (`notElem` visited) (checkPipes charList current)

findLengthOfChain :: Node -> Int
findLengthOfChain (Node c []) = 0
findLengthOfChain (Node c (node:nodes))= 1 + findLengthOfChain node

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
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day10/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day10.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day10/input.txt"
    time lines