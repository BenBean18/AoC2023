module Day16 where

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

import Data.IORef
import GHC.IO.Unsafe (unsafePerformIO)

-- Part 1
type Coord = (Int, Int)
type Direction = Coord

add :: Coord -> Coord -> Coord
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sub :: Coord -> Coord -> Coord
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

absCoord :: Coord -> Coord
absCoord (x,y) = (abs x, abs y)

sumCoord :: Coord -> Int
sumCoord (x,y) = x+y

dotTuples :: Coord -> Coord -> Int
dotTuples (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

perpendicular :: Coord -> Coord
perpendicular (x,y) = (y,-x)

-- negative y is upwards
-- positive x is to the right

rotate :: Direction -> Char -> Direction
rotate d c
  | c == '/' = if d == (1,0) then (0,-1)
        else if d == (-1,0) then (0,1)
        else if d == (0,1) then (-1,0)
        else if d == (0,-1) then (1,0)
        else trace "huh" d
  | c == '\\' = if d == (1,0) then (0,1)
        else if d == (-1,0) then (0,-1)
        else if d == (0,1) then (1,0)
        else if d == (0,-1) then (-1,0)
        else trace "huh" d
  | otherwise = trace "huh" d
x = fst
y = snd

charAt :: [String] -> Coord -> Char
charAt charList (x,y) = charList !! y !! x

{-# NOINLINE memoMap #-}
memoMap :: IORef (Map.Map (Set.Set Coord, Coord, Direction) (Set.Set Coord))
memoMap = unsafePerformIO (newIORef mempty)

followBeam' :: [[Char]] -> Set.Set Coord -> Coord -> Direction -> Set.Set Coord
followBeam' diagram visited current direction = {-(trace $ show $ Set.size visited)-} (
    if (x current < 0 || x current >= (length (head diagram)) || y current < 0 || y current >= (length diagram)) then visited else
        let thisChar = charAt diagram current in
            if thisChar == '.' then followBeam diagram (Set.insert current visited) (current `add` direction) direction
            else if thisChar == '/' then let newDirection = rotate direction '/' in followBeam diagram (Set.insert current visited) (current `add` newDirection) newDirection
            else if thisChar == '\\' then let newDirection = rotate direction '\\' in followBeam diagram (Set.insert current visited) (current `add` newDirection) newDirection
            else if thisChar == '-' then
                if direction `dotTuples` (1,0) == 0 then Set.union (followBeam diagram (Set.insert current visited) (current `add` (1,0)) (1,0)) (followBeam diagram (Set.insert current visited) (current `add` (-1,0)) (-1,0))
                else followBeam diagram (Set.insert current visited) (current `add` direction) direction
            else if thisChar == '|' then
                if direction `dotTuples` (0,1) == 0 then Set.union (followBeam diagram (Set.insert current visited) (current `add` (0,1)) (0,1)) (followBeam diagram (Set.insert current visited) (current `add` (0,-1)) (0,-1))
                else followBeam diagram (Set.insert current visited) (current `add` direction) direction
            else (trace "wtf") visited)

followBeam :: [[Char]] -> Set.Set Coord -> Coord -> Direction -> Set.Set Coord
followBeam diagram visited current direction = (trace $ show current ++ " " ++ show direction)
    unsafePerformIO $ do
        currentTable <- readIORef memoMap
        let returnedGood = Map.findWithDefault (Set.empty) (visited, current, direction) currentTable
        let returned = if returnedGood /= Set.empty then returnedGood else followBeam' diagram visited current direction
        let newMemoTable = Map.insert (visited,current,direction) returned currentTable
        -- putStrLn "wrote io ref"
        -- print (a1,a2)
        -- print (Map.size newMemoTable)
        writeIORef memoMap newMemoTable
        return returned

-- 244 too low

part1' lines =
    print $ Set.size (followBeam lines Set.empty (0,0) (1,0))

part1 = do
    lines <- getLines "day16/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day16/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day16.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day16/input.txt"
    time lines