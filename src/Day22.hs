module Day22 where

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

-- Part 1
-- Strategy: sort bricks by their Z coordinate
-- While there are bricks left, move the lowest Z bricks down as far as they can go (on top of the current highest Z block in the span of the brick)
-- Mark each "claimed" coordinate with the index of the brick

-- x,y,z all inclusive ranges
data Brick = Brick { x :: (Int, Int), y :: (Int, Int), z :: (Int, Int), i :: Int } deriving (Eq, Show, Ord)
type Grid = Map.Map (Int,Int,Int) Int -- :( the int is for which brick the spot is claimed by

parseBrick :: [String] -> Int -> Brick
parseBrick strs index = 
    let s = strs !! index
        [start,end] = splitOn "~" s
        [x1,y1,z1] = map (\num -> read num :: Int) (splitOn "," start)
        [x2,y2,z2] = map (\num -> read num :: Int) (splitOn "," end) in Brick { x = (x1,x2), y = (y1,y2), z = (z1,z2), i = index }

parseBricks :: [String] -> [Brick]
parseBricks strs = map (parseBrick strs) [0..length strs-1]

sortBricks :: [Brick] -> [Brick]
sortBricks = sortBy (\brick1 brick2 -> compare (fst (z brick1)) (fst (z brick2)))

spacesUnder :: Brick -> Grid -> Grid
spacesUnder Brick { x = (x1,x2), y = (y1,y2), z = (z1,z2), i = index } =
    Map.filter (\(x,y,z) -> x1 <= x && x <= x2 && y1 <= y && y <= y2 && z1 <= z && z <= z2)

-- Returns (z, [brick IDs])
highestBricks :: Grid -> (Int, [Int])
highestBricks grid =
    let highestZ = maximum (1 : (map (\(x,y,z) -> z) (Map.keys grid)))
        highestBrickIDs = Map.elems (Map.filter (\(x,y,z) -> z == highestZ) grid) in (highestZ, highestBrickIDs)

insertBrick :: Grid -> Brick -> Int -> Grid
insertBrick grid Brick { x = (x1,x2), y = (y1,y2), z = (z1,z2), i = index } insertionZ =
    let allX = [x1..x2]
        allY = [y1..y2]
        allZ = [0+insertionZ..(z2-z1)+insertionZ]
        allCoords = map (\x -> map (\y -> map (\z -> (x,y,z)) allZ) allY) allX -- yay 3D "for loop" in
    foldl (\m coord -> Map.insert coord index m) grid allCoords

-- returns (new grid, [supporting brick IDs])
insertFallingBrick :: Grid -> Brick -> (Grid, [Int])
insertFallingBrick grid brick =
    let (highestZ, ids) = highestBricks (spacesUnder brick grid) in (insertBrick grid brick (highestZ + 1), ids)

-- bricks MUST be sorted lowest to highest Z
makeBricksFall :: [Brick] -> (Grid, Set.Set Int)
makeBricksFall bricks = foldl (\(grid, supporting) brick -> (fst (insertFallingBrick grid brick), Set.union supporting (Set.fromList (snd (insertFallingBrick grid brick))))) (Map.empty, Set.empty) bricks

part1' lines = 
    let bricks = parseBricks lines
        sortedBricks = sortBricks bricks
        (fallenState, supportingBricks) = makeBricksFall sortedBricks
        safeToDisintegrate = length bricks - (Set.size supportingBricks) in do
    print fallenState
    print supportingBricks
    putStrLn "-------------------"
    print safeToDisintegrate


part1 = do
    lines <- getLines "day22/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day22/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day22.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day22/input.txt"
    time lines