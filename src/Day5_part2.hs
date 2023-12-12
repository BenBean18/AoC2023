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

rangeLength :: Range -> Int
rangeLength (Range start end) = abs (end - start)

rangeIntersection :: Range -> Range -> Range
rangeIntersection (Range s1 e1) (Range s2 e2) =
    if s2 > e1 || s1 > e2 then (Range 0 0) else Range (max s1 s2) (min e1 e2)

isWithin :: Range -> Int -> Bool
isWithin (Range start end) i = start <= i && i <= end

-- correspondingRange :: Range -> (Range, Range) -> [(Range, Range)]
-- correspondingRange (Range start end) (Range keyStart keyEnd, Range valStart valEnd) = 
--     let boundStart = clamp keyStart keyEnd start
--         boundEnd = clamp keyStart keyEnd end
--         newRange = (Range boundStart boundEnd, Range (valStart + boundStart - keyStart) (valStart + boundStart - keyStart + (boundEnd - boundStart))) in
--             (if keyStart < boundStart then [(Range keyStart boundStart, Range valStart (valStart + boundStart - keyStart))] else []) ++ [newRange] ++
--             (if keyEnd > boundEnd then [(Range boundEnd keyEnd, Range (valStart + (boundEnd - keyStart)) valEnd)] else [])

-- must be in bounds
correspondingRange :: Range -> (Range, Range) -> Range
correspondingRange (Range start end) (Range keyStart keyEnd, Range valStart valEnd) =
    if (rangeLength (rangeIntersection (Range start end) (Range keyStart keyEnd))) == 0 then (Range 0 0) else
        Range (valStart + (start - keyStart)) (valStart + (end - keyStart))

applyMap :: Range -> RangeMap -> [Range]
applyMap range rangeMap =
    let keys = Map.keys rangeMap
        vals = Map.elems rangeMap
        intersectingRanges = map (rangeIntersection range) keys
        mappedRanges = filter (\r -> rangeLength r /= 0) (concat (map (\inter -> map (correspondingRange inter) (zip keys vals)) (filter (\r -> rangeLength r /= 0) intersectingRanges))) in
            if length mappedRanges > 0 then mappedRanges else [range]

parseLine :: String -> RangeMap -> RangeMap
parseLine s m =
    let splot = splitOn " " s
        dst = read (head splot) :: Int
        src = read (splot !! 1) :: Int
        len = read (splot !! 2) :: Int in
            Map.insert (Range src (src + len - 1)) (Range dst (dst + len - 1)) m

parseLines :: String -> RangeMap
parseLines s = foldl (flip parseLine) Map.empty (splitOn "\n" s)

parseSeeds :: String -> [Range]
parseSeeds str = map (\s -> (Range (read (head (splitOn " " (head s))) :: Int) ((read (head (splitOn " " (head s))) :: Int)+(read (last (splitOn " " (head s))) :: Int)-1))) (str =~ "(\\d+) (\\d+)" :: [[String]])

applyMaps :: Range -> [RangeMap] -> [Range]
applyMaps r [] = []
applyMaps r (m:ms) = applyMap r m ++ concatMap (\range -> applyMaps range ms) (applyMap r m)

start :: Range -> Int
start (Range s _) = s

end :: Range -> Int
end (Range _ e) = e

minimumOfRanges :: [Range] -> Int
minimumOfRanges ranges = minimum (map start ranges)

-- applyMap :: RangeMap -> RangeMap -> [RangeMap]
-- applyMap existing new = 

type RangeMap = Map.Map Range Range

part2' lines text = do
    let seeds = parseSeeds (head lines)
    let maps = map parseLines (tail (map (head . splitOn "\n\n") (splitOn ":\n" text)))
    let mappedSeeds = concatMap ((flip applyMaps) maps) seeds
    let minimumLocation = minimumOfRanges mappedSeeds
    print seeds
    print minimumLocation