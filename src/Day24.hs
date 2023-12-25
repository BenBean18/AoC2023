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

scaleFactor :: Double
scaleFactor = 100000000000

parseHailstone :: String -> Hailstone
parseHailstone str =
    let [pos,vel] = splitOn "@" str
        posXYZ = map (\s -> (read s :: Double) / scaleFactor) (splitOn "," pos)
        velXYZ = map (\s -> (read s :: Double) / scaleFactor) (splitOn "," vel) in Hailstone { position = (posXYZ !! 0, posXYZ !! 1, posXYZ !! 2), velocity = (velXYZ !! 0, velXYZ !! 1, velXYZ !! 2) }

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
    let stones = map (\Hailstone { position = (x,y,z), velocity = v} -> Hailstone { position = (x - 200000000000000 / scaleFactor, y - 200000000000000 / scaleFactor, z), velocity = v }) $ map (zeroZ . parseHailstone) lines
        allPairs = pairs stones
        intersections = mapMaybe (uncurry checkIntersection) allPairs
        within = filter (isWithin (0,0,-(1/0)) ((400000000000000-200000000000000) / scaleFactor,(400000000000000-200000000000000) / scaleFactor,(1/0))) intersections in do
            -- print within
            print (length within)

-- 16728 too high :(
-- I think I'm being gotten by float precision, scaled down and got that minus one /shrug
-- trying 16727 (spoiler: that was correct)

part1 = do
    lines <- getLines "day24/input.txt"
    part1' lines

-- Part 2

-- Find the position and velocity such that a rock thrown from there will intersect every other hailstone's path
-- (times have to align as well)
-- I think I'm *very* well set up to solve this

{-
x,  y,  z  @ vx, vy, vz
19, 13, 30 @ -2, 1, -2

x + (t1)vx = 19 + (t1)(-2)
y + (t1)vy = 13 + (t1)(1)
z + (t1)vz = 30 + (t1)(-2)

-}

{-
so i've figured out that for a (position,velocity) pair ((a,b,c),(d,e,f)) to intersect for example 19, 13, 30 @ -2, 1, -2:
Divide[a-19,d+2]=Divide[b-13,e-1]=Divide[c-30,f+2]
-}

constraint :: Hailstone -> String
constraint Hailstone { position = (a,b,c), velocity = (d,e,f) } = "Divide[a-" ++ show (round (a * scaleFactor)) ++ ",d-" ++ show (round (d * scaleFactor)) ++ "]=Divide[b-" ++ show (round (b * scaleFactor)) ++ ",e-" ++ show (round (e * scaleFactor)) ++ "]=Divide[c-"++ show (round (c * scaleFactor)) ++",f-"++ show (round (f * scaleFactor)) ++"]"

-- For our thrown rock to hit three hailstones, the velocities from (1-2) and (2-3) must be equal.
-- First, we'll make a helper function to create an expression for the velocity between two hailstones (one line segment):
velocityBetween :: (Hailstone,Int) -> (Hailstone,Int) -> String
velocityBetween (Hailstone { position = (x1,y1,z1), velocity = (vx1,vy1,vz1) },i1) (Hailstone { position = (x2,y2,z2), velocity = (vx2,vy2,vz2) },i2) =
    let velocityFrom1To2 = "Divide[{{" ++ show (round (x2 * scaleFactor)) ++ "},{" ++ show (round (y2 * scaleFactor)) ++ "},{" ++ show (round (z2 * scaleFactor)) ++ "}}+Subscript[t,"++show i2++"]{{" ++ show (round (vx2 * scaleFactor)) ++ "},{" ++ show (round (vy2 * scaleFactor)) ++ "},{" ++ show (round (vz2 * scaleFactor)) ++ "}}-\\(40){{" ++ show (round (x1 * scaleFactor)) ++ "},{" ++ show (round (y1 * scaleFactor)) ++ "},{" ++ show (round (z1 * scaleFactor)) ++ "}}+Subscript[t,"++show i1++"]{{" ++ show (round (vx1 * scaleFactor)) ++ "},{" ++ show (round (vy1 * scaleFactor)) ++ "},{" ++ show (round (vz1 * scaleFactor)) ++ "}}\\(41),Subscript[t,"++show i2++"]-Subscript[t,"++show i1++"]]" in velocityFrom1To2



-- Divide[{{18},{19},{22}}+Subscript[t,2]{{-1},{-1},{-2}}-\(40){{19},{13},{30}}+Subscript[t,1]{{-2},{1},{-2}}\(41),Subscript[t,2]-Subscript[t,1]]=Divide[{{20},{25},{34}}+Subscript[t,3]{{-2},{-2},{-4}}-\(40){{18},{19},{22}}+Subscript[t,2]{{-1},{-1},{-2}}\(41),Subscript[t,3]-Subscript[t,2]]
-- Divide[{{x2},{y2},{z2}}+Subscript[t,2]{{vx2},{vy2},{vz2}}-\(40){{x1},{y1},{z1}}+Subscript[t,1]{{vx1},{vy1},{vz1}}\(41),Subscript[t,2]-Subscript[t,1]]

-- ok this is kind of cheating but i can't figure out for the life of me how to get this into a matrix equation form to solve myself
generateMathematicaQuery :: [Hailstone] -> String
generateMathematicaQuery stones =
    let stonePairs = pairs (zip stones [0..length stones-1])
        constraints = map (uncurry velocityBetween) stonePairs in init (foldl (\str constraint -> str ++ constraint ++ "=") "Solve [" constraints) ++ "]"

part2' lines =
    putStrLn $ generateMathematicaQuery (map parseHailstone lines)

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