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

import Numeric.LinearAlgebra

-- Part 1
-- Each intersection can be set up as a system of equations and solved using linear algebra

data Hailstone = Hailstone { position :: (Double,Double,Double), velocity :: (Double,Double,Double) } deriving (Eq, Ord, Show)

parseHailstone :: String -> Hailstone
parseHailstone str =
    let [pos,vel] = splitOn "@" str
        posXYZ = map (\s -> read s :: Double) (splitOn "," pos)
        velXYZ = map (\s -> read s :: Double) (splitOn "," vel) in Hailstone { position = (posXYZ !! 0, posXYZ !! 1, posXYZ !! 2), velocity = (velXYZ !! 0, velXYZ !! 1, velXYZ !! 2) }

{-
(ignoring Z for part 1)
19, 13, 30 @ -2, 1, -2
18, 19, 22 @ -1, -1, -2

19 - 2a = 18 - 1b --> sub r1 on both sides --> 0 = -1 - 1b + 2a --> put coefficient on one side --> 1 = -1b + 2a
(h1px - h2px) = (-h1vx)a + (h2vx)b

13 + 1a = 19 - 1b --> sub r1 on both sides --> 0 = 6 - 1b - 1a --> put coefficient on one side --> -6 = -1b - 1a
 _        _
| -1  2 |b   | 1
| -1 -1 |a = | -6

[1 b a] is in the nullspace of A
or... Ax=b --> x = A_pinv * b = inv(A'A) * A' * b

--> (The hailstones themselves don't have to collide, just test for intersections between the paths they will trace.) <-- times don't have to equal
-}

getX (x,y,z) = x
getY (x,y,z) = y
getZ (x,y,z) = z

checkIntersection :: Hailstone -> Hailstone -> Maybe (Double,Double,Double)
checkIntersection Hailstone { position = p1, velocity = v1 } Hailstone { position = p2, velocity = v2 } =
    let (xLHS, xRHS) = ([(-getX v1), getX v2], [getX p1 - getX p2])
        (yLHS, yRHS) = ([(-getY v1), getY v2], [getY p1 - getY p2])
        (zLHS, zRHS) = ([(-getZ v1), getZ v2], [getZ p1 - getZ p2])
        matA = fromLists [xLHS, yLHS, zLHS]
        vecB = fromLists [xRHS, yRHS, zRHS]
        matA_pinv = pinv matA
        vecX_times = matA_pinv Numeric.LinearAlgebra.<> vecB
        times = toList $ head $ toColumns vecX_times
        in
        if not (all (>0) times) then Nothing
        else Just (getX p1 + (times !! 0) * getX v1, getY p1 + (times !! 0) * getY v1, getZ p1 + (times !! 0) * getZ v1)

-- https://stackoverflow.com/questions/34044366/how-to-extract-all-unique-pairs-of-a-list-in-haskell
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

zeroZ :: Hailstone -> Hailstone
zeroZ Hailstone { position = (px,py,pz), velocity = (vx,vy,vz) } = Hailstone { position = (px,py,0), velocity = (vx,vy,0) }

isWithin :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double) -> Bool
isWithin (xmin, ymin, zmin) (xmax, ymax, zmax) (x,y,z) = xmin <= x && x <= xmax && ymin <= y && y <= ymax && zmin <= z && z <= zmax

part1' lines =
    let stones = map (zeroZ . parseHailstone) lines
        allPairs = pairs stones
        intersections = mapMaybe (uncurry checkIntersection) allPairs
        within = filter (isWithin (200000000000000,200000000000000,-(1/0)) (400000000000000,400000000000000,(1/0))) intersections in do
            print (length within)

-- 16728 too high :(

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