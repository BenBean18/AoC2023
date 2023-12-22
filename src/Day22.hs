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
    Map.filterWithKey (\(x,y,z) _ -> x1 <= x && x <= x2 && y1 <= y && y <= y2)

-- Returns (z, [brick IDs])
highestBricks :: Grid -> (Int, [Int])
highestBricks grid =
    let highestZ = maximum (0 : (map (\(x,y,z) -> z) (Map.keys grid)))
        highestBrickIDs = Map.elems (Map.filterWithKey (\(x,y,z) _ -> z == highestZ) grid) in (highestZ, highestBrickIDs)

insertBrick :: Grid -> Brick -> Int -> Grid
insertBrick grid Brick { x = (x1,x2), y = (y1,y2), z = (z1,z2), i = index } insertionZ =
    let allX = [x1..x2]
        allY = [y1..y2]
        allZ = [0+insertionZ..(z2-z1)+insertionZ]
        allCoords = concatMap (\x -> concatMap (\y -> map (\z -> (x,y,z)) allZ) allY) allX in -- yay 3D "for loop"
    {-(trace $ "Inserting brick " ++ show index ++ " at z=" ++ show insertionZ ++ ", coords=" ++ show allCoords ++ " new map =" ++ show (foldl (\m coord -> Map.insert coord index m) grid allCoords))-} foldl (\m coord -> Map.insert coord index m) grid allCoords

-- returns (new grid, [supporting brick IDs])
insertFallingBrick :: Grid -> Brick -> (Grid, Set.Set Int)
insertFallingBrick grid brick =
    let (highestZ, ids) = highestBricks (spacesUnder brick grid) in (trace $ "Brick " ++ show (i brick) ++ " is supported by " ++ show ids) (insertBrick grid brick (highestZ + 1), Set.fromList ids)

-- bricks MUST be sorted lowest to highest Z
makeBricksFall :: [Brick] -> (Grid, Set.Set Int)
makeBricksFall = foldl (\(grid, supporting) brick ->
        let (newGrid, supportingIDs) = insertFallingBrick grid brick in
            (newGrid, Set.union supporting (if Set.size supportingIDs == 1 then supportingIDs else Set.empty))) (Map.empty, Set.empty)

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