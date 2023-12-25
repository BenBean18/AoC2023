module Day21_part2_old where

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

{-# NOINLINE memo #-}
memo :: IORef (Map.Map Coord Bool)
memo = unsafePerformIO (newIORef mempty)

canGo :: [String] -> Coord -> Bool
canGo lines c_ = unsafePerformIO $ do
    let c = c_ `modCoord` (length (head lines), length lines)
    memoMap <- readIORef memo
    let current = Map.lookup c memoMap
    if isNothing current then do
        let val = canGo' lines c
        let newMap = Map.insert c val memoMap
        writeIORef memo newMap
        return val
    else return (fromJust current)

canGo' :: [String] -> Coord -> Bool
canGo' chars c = charAt chars c /= '#'

{-# NOINLINE neighMemo #-}
neighMemo :: IORef (Map.Map Coord [Coord])
neighMemo = unsafePerformIO (newIORef mempty)

neighboringCoords :: [String] -> Coord -> [Coord]
neighboringCoords lines c = unsafePerformIO $ do
    memoMap <- readIORef neighMemo
    let current = Map.lookup c memoMap
    if isNothing current then do
        let val = neighboringCoords' lines c
        let newMap = Map.insert c val memoMap
        writeIORef neighMemo newMap
        return val
    else return (fromJust current)

neighboringCoords' :: [String] -> Coord -> [Coord]
neighboringCoords' chars c =
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
-- ... ok so 26501365 must be significant in some way
-- probably, it just seems so random
-- prime factorization according to wolfram alpha: 5×11×481843
-- with the infinite grid, we can just do modulo on the indices to check if a value is in bounds
-- maybe this is core to how you do it?

-- can we instead of a Set do a Map.Map Coord Int (to store the # of occurrences)?
-- but then how do we see if a coordinate has been visited twice?
-- could try Set.Set (Coord, Coord) where the first coordinate is the position in the map and the second coordinate is the position OF the map (on the infinite grid)
-- but then we are still storing the same number of things

-- note: each square can only be reached in either an even OR odd number of steps
-- why? we can definitely prove that starting with even or odd, you can use any even or odd path above that (since you can move 1 away from start then back = +2 which doesn't change parity)

-- will be degree [length of the vector] - 1 since that works
designMatrix :: (Numeric t) => [t] -> Matrix t
designMatrix xValues =
    let columns = map (\exp -> map (^ exp) xValues) [0..3] in
        fromColumns (map fromList columns)

-- Y = Xb
-- or... X_pseudoinverse * Y = b
-- parameters: x, y, best fit parameters
linearRegression :: (Numeric t, Field t, Show t) => [t] -> [t] -> (t -> t)
linearRegression x y =
    let mX = designMatrix x
        vY = fromList y
        mX_pseudoinverse = pinv mX
        vBeta = mX_pseudoinverse #> vY
        beta = toList vBeta in (trace $ show mX ++ " " ++ show vBeta) (applyPolynomial beta)

applyPolynomial :: (Numeric t, Show t) => [t] -> t -> t
applyPolynomial beta x = sum (map (\i -> ((beta !! i) * (x ^ i))) [0..length beta-1])

findReachableList :: [String] -> [(Int, Int)] -> (Int, Set.Set Coord) -> Int -> [(Int, Int)]
findReachableList lines currentInts lastSet 0 = currentInts
findReachableList lines currentInts lastSet nLeft = {-(trace $! show nLeft) $!-}
    let (lastI, lastEvaluated) = lastSet
        newSet = (lastI + 1, findNumReachable lines lastEvaluated 1)
        newInts = (fst newSet, Set.size (snd newSet)) : currentInts in (trace $ show (head newInts)) findReachableList lines newInts newSet (nLeft-1)

part2' :: [String] -> IO ()
part2' lines =
    let startCoord = findStart lines 0
        ir = findReachableList lines [(0,0)] (0, Set.singleton startCoord) 1000
        -- xMean = 10000 --(sum (map fst ir_) / (fromIntegral (length ir_)))
        -- xSD = sqrt $ (sum $ map (\(i,r) -> (i - xMean) * (i - xMean)) ir_) / (fromIntegral (length ir_))
        -- ir = map (\(i,r) -> ((i - xMean) / xMean, r)) ir_
        -- poly = linearRegression (map fst ir) (map snd ir) in do
            in do
        -- print reachable
        -- print (designMatrix (map fst ir))
        -- print "---"
        putStrLn (concatMap (\(i,s) -> show i ++ "," ++ show s ++ "\n") ir)
        -- print ((26501365 - xMean) / xMean)
        -- print (poly ((26501365 - xMean) / xMean))

part2 = do
    lines <- getLines "day21/input.txt"
    part2' lines