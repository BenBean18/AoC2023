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

spacesUnder :: Brick -> Grid -> Brick
spacesUnder Brick { x = (x1,x2), y = (y1,y2), z = (z1,z2), i = index } = -- filter map's keys

part1' lines = 
    let bricks = parseBricks lines
        sortedBricks = sortBricks bricks in print sortedBricks

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