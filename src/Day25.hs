module Day25 where

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

import System.IO
import System.Random

import GHC.IO.Unsafe

import Data.List.Unique (allUnique, uniq)

-- Karger's algorithm cares about edges, so uding adjacency lists is bad
-- Store the graph as a list of edges instead
type Graph = [(String, String)]

-- Use Karger's algorithm to find the min-cut
-- Stop once a cut with 3 edges is found

contractEdge :: Graph -> String -> String -> Graph
contractEdge g a_ b_ = 
    let e = (min a_ b_, max a_ b_)
        newNode = a_ ++ b_
        deleted = removeEdge g a_ b_
        replaced = map (\(a,b) -> if (min a b) == a_ || (min a b) == b_ then (min newNode (max a b), max newNode (max a b)) else if (max a b) == a_ || (max a b) == b_ then (min (min a b) newNode, max (min a b) newNode) else (a,b)) deleted in
        replaced

removeEdge :: Graph -> String -> String -> Graph
removeEdge g a_ b_ =
    let deleted = filter (/= (min a_ b_, max a_ b_)) g in deleted

numEdgesLeft :: Graph -> Int
numEdgesLeft g = length g

edges :: Graph -> [(String, String)]
edges = id

nodes :: Graph -> [String]
nodes g = nub $ concatMap (\(a,b)->[a,b]) (edges g)

-- ...wait the brute forcer finished????????

-- https://stackoverflow.com/a/25923941
atRandIndex :: [a] -> Int -> a
atRandIndex l int = unsafePerformIO $ do
    i <- randomRIO (0, length l - 1)
    return $ l !! ((i + int) `mod` (length l))

karger :: Graph -> Int -> [(String, String)]
karger g i = {-(trace $ show (numEdgesLeft g)) $ -}if length (edges g) == 3 then edges g else
    if length (edges g) > 0 then
        let edge = atRandIndex (edges g) i
            newGraph = uncurry (contractEdge g) edge in {-(trace $ "removing " ++ show edge)-} karger newGraph i
    else (if length (edges g) == 3 then edges g else {-(trace $ show g)-} [])

-- the three edges to cut will likely be the most visited ones when pathing from any node to any other node

{-
a: b
b: c d
d: e c
e: f g
-}

parseLine :: Graph -> String -> Graph
parseLine g s =
    let [a,bs_] = splitOn ": " s
        bs = splitOn " " bs_ in foldl (\graph b -> graph ++ [(min a b, max a b)]) g bs

parseLines :: [String] -> Graph
parseLines = foldl parseLine []

findValid :: Graph -> Int -> Int
findValid g i = (trace $ show i) $ 
    let k = karger g i in if k /= [] then product $ head $ map (\(a,b) -> map (\s -> length s `div` 3) [a,b]) k else findValid g (i+1)

-- Part 1
part1' lines =
    let graph = parseLines lines
        cuts = filter (/= []) $ map (karger graph) [0..100] in do
            -- print graph
            -- print $ foldl (\g e -> foldl (\gr (ar, br) -> contractEdge gr ar br) g (filter (/= e) (edges g))) graph [("hfx","pzl"), ("bvb","cmg"), ("nvd","jqt")]
            -- print $ contractEdge (contractEdge (contractEdge graph "e" "f") "d" "ef") "def" "g"
            print $ findValid graph 0
            -- print $ graph
            -- print $ contractEdge graph "e" "f"
            -- print (map (\(a,b) -> map (\s -> length s `div` 3) [a,b]) cut)

-- MAYBE [3021, 1482]

part1 = do
    lines <- getLines "day25/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day25/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day25.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day25/input.txt"
    time lines