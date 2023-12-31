module Day20 where

import Utilities
import Data.List.Split
import Data.List
-- import Data.Text (pack, unpack, replace, isInfixOf)
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

{-
Flip-flop modules (prefix %) are either on or off; they are initially off. If a flip-flop module receives a high pulse, it is ignored and nothing happens. However, if a flip-flop module receives a low pulse, it flips between on and off. If it was off, it turns on and sends a high pulse. If it was on, it turns off and sends a low pulse.

Conjunction modules (prefix &) remember the type of the most recent pulse received from each of their connected input modules; they initially default to remembering a low pulse for each input. When a pulse is received, the conjunction module first updates its memory for that input. Then, if it remembers high pulses for all inputs, it sends a low pulse; otherwise, it sends a high pulse.

So, both of these need to preserve their state (flip flop needs to remember last pulse sent, conjunction needs to remember last pulse received).

Also, I assume part 2 will be pushing the button much more than 1000 times, which means we will probably have to detect a cycle in module states
(the problem even hints at it with "This completes the cycle: a turns off, causing con to remember only low pulses and restoring all modules to their original states.")

Idea: store a list of states for all modules (as a Map.Map String ModuleState) and a queue of pulses to process
-}

{-
This is the exit condition:
After pushing the button, you must wait until all pulses have been delivered and fully handled before pushing it again. Never push the button if modules are still processing pulses.
-}

data Pulse = Pulse { source :: String, destination :: String, isHigh :: Bool } deriving (Eq, Show, Ord)
data ModuleState = FlipFlopState Bool | ConjunctionState (Map.Map String Bool) deriving (Eq, Show, Ord)
data CircuitState = CircuitState { pulses :: [Pulse], states :: Map.Map String ModuleState, destMap :: Map.Map String [String] } deriving (Eq, Show, Ord)

processState :: CircuitState -> Int -> Int -> (CircuitState, Int, Int)
processState CircuitState { pulses = [], states = stateMap, destMap = dMap } nHigh nLow = (CircuitState { pulses = [], states = stateMap, destMap = dMap }, nHigh, nLow)
processState CircuitState { pulses = pulse:otherPulses, states = stateMap, destMap = dMap } nHigh nLow = {-(trace $show pulse)$-}
    let dest = destination pulse
        (newDestState, newPulses) = processPulse pulse (stateMap Map.! dest) dMap
        newStates = Map.insert dest newDestState stateMap in
            processState CircuitState { pulses = otherPulses ++ newPulses, states = newStates, destMap = dMap } (nHigh + if isHigh pulse then 1 else 0) (nLow + if isHigh pulse then 0 else 1)

processPulse :: Pulse -> ModuleState -> Map.Map String [String] -> (ModuleState, [Pulse])
processPulse Pulse { source = src, destination = dest, isHigh = high } (FlipFlopState current) dMap =
    if high then (FlipFlopState current, []) else (FlipFlopState (not current), [Pulse { source = dest, destination = d, isHigh = not current } | d <- dMap Map.! dest])
processPulse Pulse { source = src, destination = dest, isHigh = high } (ConjunctionState current) dMap =
    let newState = Map.insert src high current in
     (ConjunctionState newState, [Pulse { source = dest, destination = d, isHigh = not (and (Map.elems newState)) } | d <- dMap Map.! dest])

-- conjunction needs to be initialized with all LOW
-- since right now if you just have one high it goes to low

sourcesFor :: [String] -> String -> [String]
sourcesFor lines name = map (tail . head . splitOn " -> ") (filter ((" " ++ name) `isInfixOf`) lines)

-- &gk -> vq, vv, br, zt, dj, xg
parseDestinations :: String -> [String]
parseDestinations s =
    let splot = splitOn " -> " s in splitOn ", " (splot !! 1)

parseLine :: [String] -> CircuitState -> String -> CircuitState -- foldl compatible
parseLine lines CircuitState { pulses = [], states = stateMap, destMap = dMap } line =
    let (modType:rest) = line
        name = head (splitOn " -> " rest) in
        if modType == 'b' then CircuitState { pulses = [], states = stateMap, destMap = Map.insert "broadcaster" (parseDestinations line) dMap }
        else if modType == '&' then CircuitState { pulses = [], states = Map.insert name (ConjunctionState (Map.fromList (map (\s -> (s, False)) (sourcesFor lines name)))) stateMap, destMap = Map.insert name (parseDestinations line) dMap }
        else CircuitState { pulses = [], states = Map.insert name (FlipFlopState False) stateMap, destMap = Map.insert name (parseDestinations line) dMap }

emptyState :: CircuitState
emptyState = CircuitState { pulses = [], states = Map.empty, destMap = Map.empty }

parseLines :: [String] -> CircuitState
parseLines lines = foldl (parseLine lines) emptyState lines

-- Part 1
part1' lines =
    let initialState_ = parseLines lines
        initialPulses = [Pulse { source = "broadcaster", destination = d, isHigh = False } | d <- destMap initialState_ Map.! "broadcaster"]
        initialState = initialState_ { pulses = initialPulses, destMap = Map.insert "rx" [] (destMap initialState_), states = Map.insert "rx" (FlipFlopState False) (states initialState_) }
        -- 1 since we start with an initial low pulse from the button
        (finalState, finalHigh, finalLow) = foldl (\(s, h, l) i -> {-(trace $ show h ++ " " ++ show l) $ -}processState (s { pulses = initialPulses }) h (l+1)) (initialState, 0, 0) (replicate 1000 0)
        {-finalState = processState initialState 0 0-} in do
            -- print finalState
            print (finalHigh * finalLow)

part1 = do
    lines <- getLines "day20/input.txt"
    part1' lines

-- Part 2
processStateEarlyExit :: CircuitState -> Pulse -> (CircuitState, Bool)
processStateEarlyExit CircuitState { pulses = [], states = stateMap, destMap = dMap } pExit = (CircuitState { pulses = [], states = stateMap, destMap = dMap }, False)
processStateEarlyExit CircuitState { pulses = pulse:otherPulses, states = stateMap, destMap = dMap } pExit = {-(trace $show pulse)$-}
    let dest = destination pulse
        (newDestState, newPulses) = processPulse pulse (stateMap Map.! dest) dMap
        newStates = Map.insert dest newDestState stateMap in
            if pulse == pExit then (CircuitState { pulses = otherPulses ++ newPulses, states = newStates, destMap = dMap }, True)
            else processStateEarlyExit CircuitState { pulses = otherPulses ++ newPulses, states = newStates, destMap = dMap } pExit

-- checkRX :: [String] -> [Pulse] -> CircuitState -> Int -> Int
-- checkRX lines initialPulses s i = {-(trace $ show i)-} (
--     let (newState,exit) = processStateEarlyExit (s { pulses = initialPulses }) in
--         if exit then (i+1)
--         else checkRX lines initialPulses newState (i+1))

findNodesTo :: Map.Map String [String] -> String -> [String]
findNodesTo m s = Map.keys $ Map.filter (s `elem`) m

findSecondLevelNodes :: CircuitState -> (String, [String])
findSecondLevelNodes CircuitState { states = stateMap, destMap = m } = 
    -- note: on inputs that don't follow this pattern, these "assertions" will effectively fail since the patterns don't match
    let [strConnectedToRX] = findNodesTo m "rx"
        (ConjunctionState _) = stateMap Map.! strConnectedToRX
        (ConjunctionState _) = stateMap Map.! strConnectedToRX in
            -- The single node connected to rx is a conjunction node, so to send a low pulse to rx, it must be sent high pulses from all connected nodes (this should be simulate-able)
            -- SO, we find the amount of button presses required for each to send a high pulse (find the pulse twice to verify a cycle exists)
            -- ok we should try to find the cycle twice, but once worked, not exactly sure why
            (strConnectedToRX, findNodesTo m strConnectedToRX)

findCycle :: [String] -> [Pulse] -> CircuitState -> Pulse -> Int -> Int -> (CircuitState, Int)
findCycle lines initialPulses state desiredPulse cycleLength currentCycle =
    let (newState,exit) = processStateEarlyExit (state { pulses = initialPulses }) desiredPulse in
        if exit then (state, (currentCycle+1))
        else {-(trace $ show currentCycle)-} findCycle lines initialPulses newState desiredPulse cycleLength (currentCycle+1)

findCycles :: [String] -> [Pulse] -> CircuitState -> [Int]
findCycles lines initialPulses state =
    let (lastNode, secondLevelNodes) = findSecondLevelNodes state
        neededPulses = map (\s -> Pulse { source = s, destination = lastNode, isHigh = True }) secondLevelNodes
        cycles = map (\p -> snd $ findCycle lines initialPulses state p 0 0) neededPulses in cycles

-- nodes linked to hp (last before rx) and number before high pulse:
-- sn: 3967
-- sr: 3923
-- rf: 4021
-- vq: 3917
-- least common multiple (when all high pulses are sent): 245114020323037

{-
Some thinking:

If there is a cycle, it's not within 1 million iterations (since I got OOM killed at 954xxx and a cycle MUST include a low pulse sent to rx if it is ever going to happen)
So cycle detection probably won't work
Either a ton of optimization to evaluate super quickly, or some smart working backwards solution
I'm liking working backwards, but that seems horrible to evaluate
For rx to be sent a low pulse, hp must be sent all high pulses (conjunction)
For hp to be sent a high pulse, sn, rf, vq, and sr must all send high pulses sequentially (no low in between)
LCM or something?
.........this sucks

Dynamic programming could be great here, so we can try memoizing
-}

part2' lines =
    let initialState_ = parseLines lines
        initialPulses = [Pulse { source = "broadcaster", destination = d, isHigh = False } | d <- destMap initialState_ Map.! "broadcaster"]
        initialState = initialState_ { pulses = initialPulses, destMap = Map.insert "rx" [] (destMap initialState_), states = Map.insert "rx" (FlipFlopState False) (states initialState_) }
        -- 1 since we start with an initial low pulse from the button
        -- (firstState, count) = (findCycle lines initialPulses initialState (Pulse { source = "rf", destination = "hp", isHigh = True }) 0 0)
        -- (nextState, count2) = (findCycle lines initialPulses initialState (Pulse { source = "rf", destination = "hp", isHigh = False }) 0 0 0)
        -- (nextState, True) = processStateEarlyExit (initialState { pulses = initialPulses }) Pulse { source = "rf", destination = "hp", isHigh = True }
        cycles = (findCycles lines initialPulses initialState)
        leastCommonMultiple = foldl lcm (head cycles) (tail cycles)
        {-finalState = processState initialState 0 0-} in do
            -- putStrLn "Number of button presses to reach each second level node:"
            -- print cycles
            -- putStrLn "Solution (LCM of cycles):"
            print leastCommonMultiple

part2 = do
    lines <- getLines "day20/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day20.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day20/input.txt"
    time lines