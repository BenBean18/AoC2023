module Day9_old where

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

-- soooo funny analytical solution idea
-- this is basically just derivatives
-- so you just make a polynomial n-1 terms long then do a linear regression
-- and then that lets you predict the next one

-- will be degree [length of the vector] - 1 since that works
designMatrix :: (Numeric t) => [t] -> Matrix t
designMatrix xValues =
    let columns = replicate (fromIntegral $ length xValues) 1 : map (\exp -> map (^ exp) xValues) [1..(fromIntegral $ length xValues)] in
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
        beta = toList vBeta in (trace $ show (toColumns mX)) (applyPolynomial beta)

applyPolynomial :: (Numeric t, Show t) => [t] -> t -> t
applyPolynomial beta x = sum (map (\i -> {-(trace $ show (beta !! i) ++ " " ++ show i ++ " " ++ show x ++ " " ++ show ((beta !! i) * (x ^ i)))-} ((beta !! i) * (x ^ i))) [0..length beta-1])

-- 20^20 is huge, so we're scaling x values down instead
convertX :: Double -> Double
convertX a = a / 19

extrapolateFloating :: [Double] -> Double
extrapolateFloating history = linearRegression (map convertX [0..(fromIntegral $ length history-1)]) history (convertX (fromIntegral (length history)))

extrapolate :: [Double] -> Double
extrapolate history = extrapolateFloating history

extrapolateHistory :: String -> Double
extrapolateHistory s =
    let nums = map (\w -> (read w :: Double)) (words s)
        scaling = maximum (map abs nums) in
        extrapolate (map (/ scaling) nums) * scaling

-- Part 1
part1' lines =
    let extrapolatedHistories = map extrapolateHistory lines
        result = sum extrapolatedHistories in print $ round result

part1 = do
    lines <- getLines "day9/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day9/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day9.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day9/input.txt"
    time lines

-- 1789635493 too high
-- 1789635492 too high
-- 1783943934 too low