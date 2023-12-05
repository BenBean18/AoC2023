module Day5 where

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

-- Idea: Each map is a function that outputs Optional[Destination] for a source number
-- Then we store a LOT less data
type AlMap = Int -> Maybe Int

emptyMap :: AlMap
emptyMap i = Nothing

-- Format: destination source range_length
parseLine :: String -> AlMap -> AlMap
parseLine s m =
    let splot = splitOn " " s
        dst = read (splot !! 0) :: Int
        src = read (splot !! 1) :: Int
        len = read (splot !! 2) :: Int in
            (
                \i ->
                    let current = m i in
                        if isJust current then current
                        else if i >= src && i < src + len then Just (dst + (i - src))
                        else Nothing
            )

parseLines :: String -> AlMap
parseLines s = foldl (flip parseLine) emptyMap (splitOn "\n" s)



part1' lines = print "Hi"

-- Part 1
part1 = do
    lines <- getLines "day5/input.txt"
    part1' lines

part2' lines = print "Hi"

-- Part 1
part2 = do
    lines <- getLines "day5/input.txt"
    part1' lines