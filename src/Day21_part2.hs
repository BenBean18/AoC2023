module Day21_part2 where

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

type Coord = (Int, Int)
data Color = Color String | Nothing deriving (Eq, Ord, Show)

add :: Coord -> Coord -> Coord
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sub :: Coord -> Coord -> Coord
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

dotTuples :: Coord -> Coord -> Int
dotTuples (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

mul :: Coord -> Int -> Coord
mul (x,y) m = (x*m, y*m)

charAt :: [String] -> Coord -> Char
charAt charList (x,y) = charList !! y !! x

inBounds :: [String] -> Coord -> Bool
inBounds charList (xc,yc) = not (xc < 0 || yc < 0 || xc >= length (head charList) || yc >= length charList)

canGo :: [String] -> Coord -> Bool
canGo chars c = charAt chars c /= '#'

neighboringCoords :: [String] -> Coord -> [Coord]
neighboringCoords chars c =
    let allDirs = filter (inBounds chars) (map (c `add`) [(0,1),(0,-1),(1,0),(-1,0)]) in filter (canGo chars) allDirs

findStart :: [String] -> Int -> Coord
findStart lines y =
    let thisLine = lines !! y in if 'S' `elem` thisLine then (head ('S' `elemIndices` thisLine), y) else findStart lines (y+1)

-- note: (probably relevant for part 2) you don't need to find the actual tiles reached, just the # of tiles reached
findNumReachable :: [String] -> Set.Set Coord -> Int -> Set.Set Coord
findNumReachable lines currentCoords 0 = currentCoords
findNumReachable lines currentCoords stepsLeft =
    let currentList = Set.toList currentCoords
        newList = concatMap (neighboringCoords lines) currentList in findNumReachable lines (Set.fromList newList) (stepsLeft - 1)

-- Part 1
part1' lines =
    let startCoord = findStart lines 0
        reachable = findNumReachable lines (Set.singleton startCoord) 64 in do
        print reachable
        print $ Set.size reachable

part1 = do
    lines <- getLines "day21/input.txt"
    part1' lines

-- Part 2
-- ... ok so 26501365 must be significant in some way
-- probably, it just seems so random
-- prime factorization according to wolfram alpha: 5×11×481843
-- with the infinite grid, we can just do modulo on the indices to check if a value is in bounds
-- maybe this is core to how you do it?

modCoord :: Coord -> Coord -> Coord
modCoord (x,y) (mx,my) = (x `mod` mx, y `mod` my)

part2' lines = print "Hi"

part2 = do
    lines <- getLines "day21/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day21.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day21/input.txt"
    time lines