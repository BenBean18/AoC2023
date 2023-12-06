module Day5_part2 where

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

data Range = Range Int Int deriving (Ord, Show, Eq) -- start, end INCLUSIVE ON BOTH ENDS

-- should ideally assert start < end but it's ok

-- https://www.reddit.com/r/haskell/comments/22s7r2/clamp/
clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

correspondingRange :: Range -> RangeMap -> [RangeMap]
correspondingRange (Range start end) (Range keyStart keyEnd, Range valStart valEnd) = 
    let boundStart = clamp keyStart keyEnd start
        boundEnd = clamp keyStart keyEnd end
        newRange = (Range boundStart boundEnd, Range (valStart + boundStart - keyStart) (valStart + boundStart - keyStart + (boundEnd - boundStart))) in
            (if keyStart < boundStart then [(Range keyStart boundStart, Range valStart (valStart + boundStart - keyStart))] else []) ++ [newRange] ++
            (if keyEnd > boundEnd then [(Range boundEnd keyEnd, Range (valStart + (boundEnd - keyStart)) valEnd)] else [])

-- applyMap :: RangeMap -> RangeMap -> [RangeMap]
-- applyMap existing new = 

type RangeMap = (Range, Range) -- key, value

-- e.g. we have the range [1,2,3] -> [4,5,6], [4,5,6] -> [1,2,3]
-- and we get the input [3,4,5]
-- so we need to get the intersection of [3,4,5] with each of the keys
-- which should yield [3,6]

part2' lines = print "Hi"