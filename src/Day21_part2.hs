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
import Data.IORef
import GHC.IO.Unsafe

import Numeric.LinearAlgebra
    ( Numeric,
      Matrix,
      Field,
      pinv,
      (#>),
      fromColumns,
      toColumns,
      toList,
      fromList )

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

modCoord :: Coord -> Coord -> Coord
modCoord (x,y) (mx,my) = (x `mod` mx, y `mod` my)

charAt :: [String] -> Coord -> Char
charAt charList (x,y) = charList !! y !! x

inBounds :: [String] -> Coord -> Bool
inBounds charList (xc,yc) = not (xc < 0 || yc < 0 || xc >= length (head charList) || yc >= length charList)

canGo :: [String] -> Coord -> Bool
canGo chars c = charAt chars (c `modCoord` (131,131)) /= '#'

neighboringCoords :: [String] -> Coord -> [Coord]
neighboringCoords chars c =
    let allDirs = {-filter (inBounds chars) -}(map (c `add`) [(0,1),(0,-1),(1,0),(-1,0)]) in filter (canGo chars) allDirs

findStart :: [String] -> Int -> Coord
findStart lines y =
    let thisLine = lines !! y in if 'S' `elem` thisLine then (head ('S' `elemIndices` thisLine), y) else findStart lines (y+1)

-- note: (probably relevant for part 2) you don't need to find the actual tiles reached, just the # of tiles reached
findNumReachable :: [String] -> Set.Set Coord -> Int -> Set.Set Coord
findNumReachable lines currentCoords 0 = currentCoords
findNumReachable lines currentCoords stepsLeft =
    let currentList = Set.toList currentCoords
        newList = concatMap (neighboringCoords lines) currentList in findNumReachable lines (Set.fromList newList) (stepsLeft - 1)

-- Part 2
designMatrix :: (Numeric t) => [t] -> Matrix t
designMatrix xValues =
    let columns = map (\exp -> map (^ exp) xValues) [0..2] in
        fromColumns (map fromList columns)

-- Y = Xb
-- or... X_pseudoinverse * Y = b
-- parameters: x, y, best fit parameters
linearRegression :: [Double] -> [Double] -> (Int -> Int)
linearRegression x y =
    let mX = designMatrix x
        vY = fromList y
        mX_pseudoinverse = pinv mX
        vBeta = mX_pseudoinverse #> vY
        beta = toList vBeta in (trace $ show mX ++ " " ++ show vBeta) (applyPolynomial beta)

applyPolynomial :: [Double] -> Int -> Int
applyPolynomial beta x = sum (map (\i -> (round (beta !! i) * (x ^ i))) [0..length beta-1])
-- ... ok so 26501365 must be significant in some way
-- it's 202300 * 131 + 65 (had to look at reddit for this one, i was very stuck)

-- note: each square can only be reached in either an even OR odd number of steps
-- why? we can definitely prove that starting with even or odd, you can use any even or odd path above that (since you can move 1 away from start then back = +2 which doesn't change parity)

makeColorful :: String -> String
makeColorful s = "\x1b[94m" ++ s ++ "\x1b[0m"

visualizeCoordinateSet' :: [Coord] -> [[Char]] -> Int -> [[Char]]
visualizeCoordinateSet' coords strings yCoord = if yCoord == 394+131 then strings else
    let xCoordsOnThisLine = map fst $ filter (\(x,y) -> y == yCoord) coords
        -- we probably only need to visualize nine (131x131) tiles
        -- that's 393x393
        -- so -197..197 on both x and y
        thisLine = concatMap (\x -> (if x `mod` 131 == 0 || yCoord `mod` 131 == 0 then makeColorful else id) (if x `elem` xCoordsOnThisLine then "O" else ".")) [-262-131..393+131] in visualizeCoordinateSet' coords (strings ++ [thisLine]) (yCoord + 1)

visualizeCoordinateSet :: [Coord] -> [[Char]]
visualizeCoordinateSet coords = visualizeCoordinateSet' coords [[]] (-262-131)

part2' :: [String] -> IO ()
part2' lines =
    let startCoord = findStart lines 0
        reachable = map (findNumReachable lines (Set.singleton startCoord)) [65+131*0,65+131*1,65+131*2,65+131*3]
        -- note: steps = 65 + 131x, and the y axis is the number of squares reached
        (xs,ys) = (map (\x -> fromIntegral x :: Double) [0,1,2,3], map (fromIntegral . Set.size) reachable)
        poly = linearRegression xs ys
        -- for verification:
        next = findNumReachable lines (Set.singleton startCoord) (65+131*4) in do
        putStrLn $ show (poly 4) ++ "=="
        putStrLn $ show (Set.size next) ++ " for this to be correct"
        putStrLn $ "Solution: " ++ show (poly ((26501365 - 65) `div` 131))
        -- print reachable
        -- print $ Set.size reachable
        -- visualization:
        -- putStrLn $ concatMap (\(r,i) -> show i ++ unlines (visualizeCoordinateSet (Set.toList r))) (zip reachable [65,196,327,458])

-- 79667628050727568 too high, maybe floating point stuff


part2 = do
    lines <- getLines "day21/input.txt"
    part2' lines