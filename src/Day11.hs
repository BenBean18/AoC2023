module Day11 where

import Utilities
import Data.List.Split
import Data.List (elemIndices)
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

-- Use Dijkstra's to find shortest distances between pairs of points

isEmpty :: String -> Bool
isEmpty = all (== '.')

expandRows :: [String] -> [String]
expandRows [] = []
expandRows (x:xs) = if isEmpty x then x : x : expandRows xs else x : expandRows xs

columnAt :: [String] -> Int -> String
columnAt strs i = map (!! i) strs

transpose :: [String] -> [String]
transpose strs = map (columnAt strs) [0..length (head strs)-1]

expandGalaxy :: [String] -> [String]
expandGalaxy strs = expandRows (transpose (expandRows (transpose strs)))

-- ok so our graph will be a grid
-- and then we get taxicab distance for free

inBounds :: [String] -> Coord -> Bool
inBounds charList (xc,yc) = not (xc < 0 || yc < 0 || xc >= length (head charList) || yc >= length charList)

add :: Coord -> Coord -> Coord
add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

sub :: Coord -> Coord -> Coord
sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

absCoord :: Coord -> Coord
absCoord (x,y) = (abs x, abs y)

sumCoord :: Coord -> Int
sumCoord (x,y) = x+y

distanceBetween :: Coord -> Coord -> Int
distanceBetween c1 c2 = sumCoord (absCoord (c1 `sub` c2))

findGalaxies :: [String] -> Int -> [Coord]
findGalaxies lines i =
    if i >= length lines then [] else
    let xCoords = elemIndices '#' (lines !! i) in (map (\x -> (x,i)) xCoords) ++ findGalaxies lines (i+1)

-- this does both directions so we need to divide by 2 for sum
pairDistances :: [Coord] -> Int -> [Int]
pairDistances coords i = 
    if i >= length coords then [] else
    let thisCoord = coords !! i
        otherCoords = filter (/= thisCoord) coords in
        map (distanceBetween (coords !! i)) otherCoords ++ pairDistances coords (i+1)

-- Part 1
part1' lines =
    let newLines = expandGalaxy lines
        coords = findGalaxies newLines 0 in do
        -- putStrLn $ concatMap (\s -> s ++ "\n") newLines
        print (sum (pairDistances coords 0) `div` 2)

part1 = do
    lines <- getLines "day11/input.txt"
    part1' lines

-- Part 2


part2' lines = print "Hi"

part2 = do
    lines <- getLines "day11/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day11.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day11/input.txt"
    time lines