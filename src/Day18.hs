module Day18 where

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

-- yay time to use the cool graphics trick of how many walls you enter!
-- or the shoelace theorem
-- but let's just start with the graphics one

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
getCoordList' :: [String] -> [Coord]
getCoordList' = foldl (\currentList str -> head currentList `add` parseMove str : currentList) [(0,0)]

getCoordList :: [String] -> [Coord]
getCoordList lines =
    let l = getCoordList' lines in map (\c -> c `sub` (minimum (map xCoord l), minimum (map yCoord l))) l

-- shoelace formula! https://en.wikipedia.org/wiki/Shoelace_formula
polygonArea' :: [Coord] -> Int
polygonArea' [_,_] = 0
polygonArea' ((x0,y0):(x1,y1):coords) = (x0*y1 - x1*y0) + polygonArea' ((x1,y1):coords)

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

xCoord :: Coord -> Int
xCoord (x,_) = x

yCoord :: Coord -> Int
yCoord (_,y) = y

polygonArea :: [Coord] -> Int
polygonArea (coords) = (polygonArea' coords + (xCoord (last coords) * yCoord (head coords)) - (xCoord (head coords) * yCoord (last coords))) `div` 2

-- Trying again using the cool graphics wall trick

replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace f i [] = []

replace2D :: a -> (Int, Int) -> [[a]] -> [[a]]
replace2D f (x,y) = replace (replace (const f) x) y

range :: Coord -> Coord -> [Coord]
range (x1,y1) (x2,y2) = concatMap (\y -> map (\x -> (x,y)) [min x1 x2..max x1 x2]) [min y1 y2..max y1 y2]

emptyTrench :: [Coord] -> [[Bool]]
emptyTrench coords = replicate (maximum (map yCoord coords) + 1) (replicate (maximum (map xCoord coords) + 1) False)

-- Find 2D array where true = in wall, false = not in wall
buildTrench :: [Coord] -> [[Bool]] -> [[Bool]]
buildTrench [_,_,_] l = l
buildTrench (c0:c1:coords) currentList =
    let newList = foldl (\list coord -> replace2D True coord list) currentList (range c0 c1) in buildTrench (c1:coords) newList

-- Part 1
part1' lines =
    let coordList = getCoordList lines
        area = polygonArea coordList in do
        print coordList
        print (buildTrench coordList (emptyTrench coordList))
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