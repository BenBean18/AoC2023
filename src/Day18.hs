module Day18 where

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

-- yay time to use the cool graphics trick of how many walls you enter!
-- or the shoelace theorem
-- but let's just start with the graphics one

type Coord = (Int, Int)

add :: Coord -> Coord -> Coord
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sub :: Coord -> Coord -> Coord
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

dotTuples :: Coord -> Coord -> Int
dotTuples (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

mul :: Coord -> Int -> Coord
mul (x,y) m = (x*m, y*m)

getDirection :: Char -> Coord
getDirection 'U' = (0,1)
getDirection 'D' = (0,-1)
getDirection 'L' = (-1,0)
getDirection 'R' = (1,0)

-- ignoring colors for now (so about to be destroyed by part 2)
parseMove :: String -> Coord
parseMove s =
    let theWords = words s
        direction = getDirection (head (head theWords))
        magnitude = read (theWords !! 1) :: Int in direction `mul` magnitude

-- will be in reverse order
getCoordList :: [String] -> [Coord]
getCoordList = foldl (\currentList str -> head currentList `add` parseMove str : currentList) [(0,0)]

-- shoelace formula! https://en.wikipedia.org/wiki/Shoelace_formula
polygonArea' :: [Coord] -> Int
polygonArea' [_,_] = 0
polygonArea' ((x0,y0):(x1,y1):coords) = ((x0 - x1) * (y0 + y1)) + polygonArea' ((x1,y1):coords)

{-
#######
#.....#
###...#
..#...#
..#...#
###.###
#...#..
##..###
.#....#
.######
-}

polygonArea :: [Coord] -> Int
polygonArea coords = polygonArea' coords `div` 2

-- Part 1
part1' lines =
    let coordList = getCoordList lines
        area = polygonArea coordList in do
        print coordList
        print area

part1 = do
    lines <- getLines "day18/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day18/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day18.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day18/input.txt"
    time lines