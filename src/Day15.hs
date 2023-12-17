module Day15 where

import Utilities
import Data.List.Split
import Data.List
import Data.Text (pack, unpack, replace, isInfixOf)
import Text.Regex.Base
import Text.Regex.PCRE
import Data.Array ((!), Array, (//), array, elems, assocs)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Criterion.Main
import System.Environment
import Data.Maybe
import qualified Data.MultiSet as MultiSet
import Data.List.Unique (allUnique)
import Data.Char (ord)

-- Part 1
hash' :: String -> Int -> Int
hash' [] currentValue = currentValue
hash' (c:str) currentValue = let newValue = ((currentValue + (ord c)) * 17) `mod` 256 in
    hash' str newValue

hash :: String -> Int
hash str = hash' str 0

part1' lines =
    let initSequence = head lines
        result = sum (map hash (splitOn "," initSequence)) in print result

part1 = do
    lines <- getLines "day15/input.txt"
    part1' lines

-- Part 2
data Lens = Lens String Int deriving (Eq, Ord, Show)

name :: Lens -> String
name (Lens n _) = n

focalLength :: Lens -> Int
focalLength (Lens _ l) = l

emptyMap :: Array Int [Lens]
emptyMap = array (0,255) [(i, []) | i <- [0..255]]

addToMap :: Array Int [Lens] -> Lens -> Array Int [Lens]
addToMap currentMap lens =
    let hashedName = hash (name lens)
        currentList = currentMap ! hashedName
        newList = if (name lens) `elem` (map name currentList) then map (\thisLens -> if (name thisLens) == (name lens) then lens else thisLens) currentList else currentList ++ [lens] in
            currentMap // [(hashedName, newList)]

removeFromMap :: Array Int [Lens] -> String -> Array Int [Lens]
removeFromMap currentMap label =
    let hashedName = hash label in currentMap // [(hashedName, filter (\lens -> name lens /= label) (currentMap ! hashedName))]

parseLens :: String -> Lens
parseLens str =
    let splot = splitOn "=" str in Lens (head splot) (read (last splot) :: Int)

doOperation :: Array Int [Lens] -> String -> Array Int [Lens]
doOperation currentMap str =
    let newMap = if '-' `elem` str then removeFromMap currentMap (init str) else addToMap currentMap (parseLens str) in
    {-(trace $ str ++ "\n\n" ++ show newMap ++ "\n-----------------------\n")-} newMap

focusingPower :: Array Int [Lens] -> Int
focusingPower boxes = sum $ map (\(i, lenses) -> (i+1) * sum (zipWith (\lens i -> i * focalLength lens) lenses [1..(length lenses)])) (assocs boxes)

runInitialization :: String -> Array Int [Lens]
runInitialization str = foldl doOperation emptyMap (splitOn "," str) -- Functional programming is awesome :)

part2' lines =
    let initSequence = head lines
        result = focusingPower (runInitialization initSequence) in print result

part2 = do
    lines <- getLines "day15/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day15.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day15/input.txt"
    time lines