module Day19 where

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

-- Part 1
data Rating = Rating {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Eq, Show, Ord) -- very clever naming eric wastl
type WorkflowMap = Map.Map String Workflow
type Workflow = Rating -> String

isNumeric :: Char -> Bool
isNumeric c = c `elem` ['0'..'9']

-- {x=787,m=2655,a=1222,s=2876}
parseRating :: String -> Rating
parseRating s =
    let withoutBrackets = init (tail s)
        segments = splitOn "," withoutBrackets
        numbers = map (\seg -> read (filter isNumeric seg) :: Int) segments in Rating { x = numbers !! 0, m = numbers !! 1, a = numbers !! 2, s = numbers !! 3 }

getProperty :: Rating -> Char -> Int
getProperty Rating { x = x } 'x' = x
getProperty Rating { m = m } 'm' = m
getProperty Rating { a = a } 'a' = a
getProperty Rating { s = s } 's' = s

gt :: Int -> Int -> Bool
gt a b = a > b

lt :: Int -> Int -> Bool
lt a b = a < b

parseCondition :: String -> Rating -> Maybe String
parseCondition s r =
    let [conditionStr,output] = splitOn ":" s
        property = head conditionStr
        condition = if head (tail conditionStr) == '>' then gt else lt
        num = read (drop 2 conditionStr) :: Int
        in if (r `getProperty` property) `condition` num then Just output else Nothing

-- ex{x>10:one,m<20:two,a>30:R,A}
parseWorkflow :: String -> (String, Workflow)
parseWorkflow s =
    let name = head (splitOn "{" s)
        content = init (last (splitOn "{" s))
        origConditions = splitOn "," content
        defaultOutput = last origConditions
        conditions = init origConditions
        wf rating = fromJust (head (filter isJust (map (flip parseCondition rating) conditions) ++ [Just defaultOutput])) in (name, wf)

runWorkflows :: WorkflowMap -> Rating -> String -> Bool
runWorkflows _ _ "A" = True
runWorkflows _ _ "R" = False
runWorkflows m r s =
    let wf = m Map.! s in runWorkflows m r (wf r)

parseWorkflows :: [String] -> WorkflowMap
parseWorkflows lines = foldl (\m (s,w) -> Map.insert s w m) Map.empty (map parseWorkflow lines)

ratingSum :: Rating -> Int
ratingSum r = sum (map (getProperty r) "xmas")

isAccepted :: WorkflowMap -> Rating -> Bool
isAccepted m r = runWorkflows m r "in"

part1' lines =
    let [workflowsStr,ratingsStr] = splitOn [""] lines
        workflowMap = parseWorkflows workflowsStr
        ratings = map parseRating ratingsStr
        accepted = filter (isAccepted workflowMap) ratings
        score = sum (map ratingSum accepted) in do
            print score

part1 = do
    lines <- getLines "day19/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

part2 = do
    lines <- getLines "day19/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day19.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day19/input.txt"
    time lines