module Day20 where

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
processState CircuitState { pulses = pulse:otherPulses, states = stateMap, destMap = dMap } nHigh nLow = (trace $ show pulse ++ " " ++ show otherPulses) (
    let dest = destination pulse
        (newDestState, newPulses) = processPulse pulse (stateMap Map.! dest) dMap
        newStates = Map.insert dest newDestState stateMap in
            processState CircuitState { pulses = otherPulses ++ newPulses, states = newStates, destMap = dMap } (nHigh + if isHigh pulse then 1 else 0) (nLow + if isHigh pulse then 0 else 1))

processPulse :: Pulse -> ModuleState -> Map.Map String [String] -> (ModuleState, [Pulse])
processPulse Pulse { source = src, destination = dest, isHigh = high } (FlipFlopState current) dMap =
    if high then (FlipFlopState current, []) else (FlipFlopState (not current), [Pulse { source = dest, destination = d, isHigh = not current } | d <- dMap Map.! dest])
processPulse Pulse { source = src, destination = dest, isHigh = high } (ConjunctionState current) dMap =
    let newState = Map.insert src high current in
     (ConjunctionState newState, [Pulse { source = dest, destination = d, isHigh = all not (Map.elems newState) } | d <- dMap Map.! dest])

-- &gk -> vq, vv, br, zt, dj, xg
parseDestinations :: String -> [String]
parseDestinations s = 
    let splot = splitOn " -> " s in splitOn ", " (splot !! 1)

parseLine :: CircuitState -> String -> CircuitState -- foldl compatible
parseLine CircuitState { pulses = [], states = stateMap, destMap = dMap } line =
    let (modType:rest) = line
        name = head (splitOn " -> " rest) in
        if modType == 'b' then CircuitState { pulses = [], states = stateMap, destMap = Map.insert "broadcaster" (parseDestinations line) dMap }
        else if modType == '&' then CircuitState { pulses = [], states = Map.insert name (ConjunctionState Map.empty) stateMap, destMap = Map.insert name (parseDestinations line) dMap }
        else CircuitState { pulses = [], states = Map.insert name (FlipFlopState False) stateMap, destMap = Map.insert name (parseDestinations line) dMap }

emptyState :: CircuitState
emptyState = CircuitState { pulses = [], states = Map.empty, destMap = Map.empty }

parseLines :: [String] -> CircuitState
parseLines = foldl parseLine emptyState

-- Part 1
part1' lines =
    let initialState = parseLines lines
        finalState = processState CircuitState { pulses = [Pulse { source = "broadcaster", destination = d, isHigh = False } | d <- destMap initialState Map.! "broadcaster"], states = states initialState, destMap = destMap initialState } 0 0 in print finalState

part1 = do
    lines <- getLines "day20/input.txt"
    part1' lines

-- Part 2
part2' lines = print "Hi"

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