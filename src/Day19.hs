module Day19 where

import Utilities
import Data.List.Split
import Data.List ()
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

import Data.Range

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

-- Idea: "reverse" the problem by starting at A. Don't want to start at "in" because you have to consider more possibilities that way.
-- so for example, take the example input
-- find all references to A (line by line) and go all the way down back to in using a DFS
-- px{a<2006:qkq,m>2090:A,rfg}. A is reached if m > 2090, not (a < 2006), and (px)
-- now find all references to (px)
-- in{s<1351:px,qqz}. A is reached if m > 2090, not (a < 2006), and s < 1351 (and implicitly, if x is anything).
-- so that's 4000 * (4000-2090) * (4000-2006) * (1351-1) accepted ratings
-- (adding 1 because if it is x)

-- mercifully, it looks like only one thing points to each thing in the input (each node has only one way to get to it -- only one in-degree)
-- so this probably means we don't need to memoize the function since the memo will never be used?

-- next reference to A:
-- pv{a>1716:R,A} -> if x, m, a > 1716, s and pv
-- find references to pv:
-- so this is interesting, hdj accepts as well. i think we ignore and come back to it later to avoid double counting
-- hdj{m>838:A,pv} -> if x, not (m > 838), a > 1716, s, and hdj
-- find references to hdj:
-- qqz{s>2770:qs,m<1801:hdj,R} -> if x, not (m > 838), a > 1716, not (s > 2770), and qqz
-- find references to qqz:
-- in{s<1351:px,qqz} YAY -> if x, not (m > 838), a > 1716, not (s > 2770), not (s < 1351)

-- combining conditions is interesting -- how do we resolve not (s > 2770) AND not (s < 1351)
-- ...honestly probably ranges
-- intersection of (difference of [1..4000], [2770+1..4000]), (difference of [1..4000], [1..1351-1])

{-
how to find opposite valid numbers (if we need to check for NOT a condition):
intersection (invert [1 *=+ 3]) [1 +=+ 4000]
[SingletonRange 1,3 *=+ 4000]

ghci> notCondition (rangeForCondition "s>2770:bcd")
1 +=+ 2770
ghci> r1 = notCondition (rangeForCondition "s>2770:bcd")
ghci> r2 = notCondition (rangeForCondition "s<1351:fksf")
ghci> r1
1 +=+ 2770
ghci> r2
1351 +=+ 4000
ghci> union [r1] [r2]
[1 +=+ 4000]
ghci> inter
interact      intersection
ghci> intersection [r1] [r2]
[1351 +=+ 2770]
-}

rangeForCondition :: String -> ((Char, [Range Int]), String)
rangeForCondition s =
    let [conditionStr,output] = splitOn ":" s
        property = head conditionStr
        condition = head (tail conditionStr)
        num = read (drop 2 conditionStr) :: Int
        in if condition == '>' then ((property, [num *=+ 4000]), output) else ((property, [1 +=* num]), output)

notCondition :: [Range Int] -> [Range Int]
notCondition r = intersection (invert r) [1 +=+ 4000]

reachableRange :: String -> String -> (String, Map.Map Char [Range Int])
reachableRange workflow output =
    let name = head (splitOn "{" workflow)
        content = init (last (splitOn "{" workflow))
        origConditions = splitOn "," content
        defaultOutput = last origConditions
        conditionStrs = init origConditions
        conditions = map rangeForCondition conditionStrs ++ [(('x', [1 +=+ 4000]), defaultOutput)] in {-(trace $ show conditions)-} (name, fillMap $ reachableRange' conditions output defaultMap2 False)

defaultMap :: Map.Map Char [Range Int]
defaultMap = Map.fromList [('x', [1 +=+ 4000]),('m', [1 +=+ 4000]),('a', [1 +=+ 4000]),('s', [1 +=+ 4000])]

defaultMap2 :: Map.Map Char ([Range Int], [Range Int])
defaultMap2 = Map.fromList [('x', ([1 +=+ 4000],[])),('m', ([1 +=+ 4000],[])),('a', ([1 +=+ 4000],[])),('s', ([1 +=+ 4000],[]))]

emptyMap :: Map.Map Char [Range Int]
emptyMap = Map.fromList [('x', []),('m', []),('a', []),('s', [])]

invertAll :: Map.Map Char [Range Int] -> Map.Map Char [Range Int]
invertAll = Map.map invert

intersectAll :: Map.Map Char [Range Int] -> Map.Map Char [Range Int] -> Map.Map Char [Range Int]
intersectAll m1 m2 =
    let firstElems = Map.elems m1
        secondElems = Map.elems m2 in Map.fromList (zip (Map.keys m1) (zipWith intersection firstElems secondElems))

fillSet :: [Range Int] -> [Range Int]
fillSet s = if s == [] then [1 +=+ 4000] else s

intersectWithInverted :: Map.Map Char ([Range Int],[Range Int]) -> Bool -> Map.Map Char [Range Int]
intersectWithInverted m isDefaultWanted =
    let firstElems = map fst $ Map.elems m
        secondElems = map snd $ Map.elems m in Map.fromList (zip (Map.keys m) (zipWith (\a b -> (trace $ "\n" ++ show a ++ " " ++ show b ++ "\n") $ if a /= [] && b == [] && isDefaultWanted then [1 +=+ 4001] else intersection (fillSet a) (invert b)) firstElems secondElems))

reachableRange' :: [((Char, [Range Int]), String)] -> String -> Map.Map Char ([Range Int], [Range Int]) -> Bool -> Map.Map Char [Range Int]
-- go through conditions left to right
-- if rangeForCondition of the string leads to not the output then add notCondition (that range) to the list
-- if rangeForCondition of the string leads to the output then stop there
-- note: here there will be only one x, m, a, s
-- ... try having a list of yes and no conditions?
reachableRange' [] _ m found = if found then intersectWithInverted m False else defaultMap
reachableRange' (condition:conditions) name currentMap found =
    let ((p, r), out) = condition in
        if out == name then
            if r /= [1 +=+ 4000] then
                let reachableSet = (intersection (fst (currentMap Map.! p)) r)
                    unreachableSet = (snd (currentMap Map.! p)) in
                reachableRange' conditions name (Map.insert p (reachableSet, unreachableSet) currentMap) True
            else 
                -- default value. fill with everything not marked as reachable.
                {-(trace $ show currentMap)-} intersectWithInverted currentMap True
        else if not found then
            let reachableSet = union (fst (currentMap Map.! p)) (invert r)
                unreachableSet = (union (snd (currentMap Map.! p)) r) in
            reachableRange' conditions name (Map.insert p (reachableSet, unreachableSet) currentMap) found
        else {-(trace $ show currentMap)-} intersectWithInverted currentMap False

fillMap :: Map.Map Char [Range Int] -> Map.Map Char [Range Int]
fillMap m =
    let xs = if m Map.! 'x' == [] then [1 +=+ 4000] else m Map.! 'x'
        ms = if m Map.! 'm' == [] then [1 +=+ 4000] else m Map.! 'm'
        as = if m Map.! 'a' == [] then [1 +=+ 4000] else m Map.! 'a'
        ss = if m Map.! 's' == [] then [1 +=+ 4000] else m Map.! 's' in Map.fromList [('x',xs),('m',ms),('a',as),('s',ss)]


{-
ghci> reachableRange "px{a<2006:qkq,m>2090:A,rfg}" "A"
fromList [('a',[2006 +=+ 4000]),('m',[2090 *=+ 4000]),('s',[1 +=+ 4000]),('x',[1 +=+ 4000])]
ghci> reachableRange "in{s<1351:px,qqz}" "px"
fromList [('a',[1 +=+ 4000]),('m',[1 +=+ 4000]),('s',[1 +=* 1351]),('x',[1 +=+ 4000])]
-}

-- have to be careful with something like lnx{m>1548:A,A}
-- (two references to the same thing)

-- I HATE EDGE CASES
-- vdp{x>324:A,x<146:R,A}
-- uhh this one too:
-- vgh{x>1126:A,m>3898:A,ktl}
-- so now we are dealing with range sets :cry:

unifyReachableRanges :: [Map.Map Char [Range Int]] -> Map.Map Char [Range Int]
unifyReachableRanges maps = if Map.empty `elem` maps then Map.empty else
    let xs = map (Map.! 'x') maps
        ms = map (Map.! 'm') maps
        as = map (Map.! 'a') maps
        ss = map (Map.! 's') maps in Map.fromList [('x', foldl intersection [1 +=+ 4000] xs), ('m', foldl intersection [1 +=+ 4000] ms), ('a', foldl intersection [1 +=+ 4000] as), ('s', foldl intersection [1 +=+ 4000] ss)]

dfsReachableRange :: [String] -> [String] -> String -> Map.Map Char [Range Int]
dfsReachableRange _ _ "in" = defaultMap
dfsReachableRange lines ignore output =
    let occurs = filter (\line -> {-(trace $ show output ++ " " ++ show ignore ++ " " ++ show (snd (reachableRange line output)) ++ "\n") $-} snd (reachableRange line output) /= defaultMap && (output /= "A" || {-(trace $ (if line `elem` ignore then "\nignoring " ++ show line ++ " for " ++ show output ++ " " ++ show line else show line ++ " " ++ show ignore))-} line `notElem` ignore)) lines in if length occurs == 0 then Map.empty else
    let firstOccur = head occurs
        (name, rr_) = reachableRange firstOccur output
        rr = intersectAll rr_ defaultMap in {-(trace $ "selected " ++ show firstOccur ++ "\n\n\n") $-} unifyReachableRanges [rr, dfsReachableRange lines ignore name]

rangeSizeR :: Range Int -> Int
rangeSizeR (SpanRange lower upper) = boundValue upper - boundValue lower - (if boundType lower == Exclusive then 1 else 0) - (if boundType upper == Exclusive then 1 else 0) + 1

rangeSize :: [Range Int] -> Int
rangeSize rs = sum $ map rangeSizeR rs

-- 102838812800896 too low

--- and this test case fails :(
-- xd{s>382:R,x>3636:A,s>248:A,A}

numReachable :: Map.Map Char [Range Int] -> Int
numReachable m =
    let ranges = Map.elems m in product (map rangeSize ranges)



part2' lines =
    let [workflows,ratingsStr] = splitOn [""] lines
        acceptedWorkflows = filter (\workflow -> 'A' `elem` workflow) workflows -- haha
        exclusionList = map (`take` acceptedWorkflows) [0..length acceptedWorkflows-1]
        acceptancePaths = map (\toExclude -> dfsReachableRange workflows toExclude "A") exclusionList in do
        print exclusionList
        print acceptancePaths
        print (map numReachable acceptancePaths)
        print (sum (map numReachable acceptancePaths))

part2 = do
    lines <- getLines "day19/input.txt"
    part2' lines

{-
Correct is:
[20576430000000,8167885440000,43392000000000,8281393428000,35328000000000,15320205000000,14486526000000,21856640000000]
167409079868000
-}

time lines =
    withArgs ["--output", "day19.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day19/input.txt"
    time lines