module Day24 where

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
-- Each intersection can be set up as a system of equations and solved using linear algebra

data Hailstone = Hailstone { position :: (Int,Int,Int), velocity :: (Int,Int,Int) } deriving (Eq, Ord, Show)

parseHailstone :: String -> Hailstone
parseHailstone str =
    let [pos,vel] = splitOn "@" str
        posXYZ = map (\s -> read s :: Int) (splitOn "," pos)
        velXYZ = map (\s -> read s :: Int) (splitOn "," vel) in Hailstone { position = (posXYZ !! 0, posXYZ !! 1, posXYZ !! 2), velocity = (velXYZ !! 0, velXYZ !! 1, velXYZ !! 2) }

part1' lines = print "Hi"

part1 = do
    lines <- getLines "day24/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day24/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day24.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day24/input.txt"
    time lines