{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.List
import Data.Bits
import Control.Concurrent

type Cell = Bool
off = False
on = True
type RuleSet = (Cell, Cell, Cell) -> Cell
type SimulationState = [Cell]
type Simulation = (RuleSet, SimulationState)

stepSim :: Simulation -> Simulation
stepSim (rule, (c:cs)) = (rule, c : runKeepingTail (c:cs))
    where
        runKeepingTail [c1, c2] = [c2]
        runKeepingTail (c1:c2:c3:cs) = rule (c1, c2, c3) : runKeepingTail (c2:c3:cs)

-- | Creates a ruleset from a number - rule from video: 145
fromNumber :: Integer -> RuleSet
fromNumber n =  testBit n . cellTrippleToNumber
    where
        cellTrippleToNumber = sum . zipWith (*) [4, 2, 1] . cellTrippleTo01        
        cellTrippleTo01 (c1, c2, c3) = map fromEnum [c1, c2, c3]

showSim :: Simulation -> String
showSim (_, cs) = showSimState cs ++ "\n"
showSimState :: SimulationState -> String
showSimState = concatMap showCell
showCell :: Cell -> String
showCell c = if c then "🟥" else "🟨"

showRuleSet :: RuleSet -> String
showRuleSet rule = intercalate "\n" mappings ++ "\n"
    where
        mappings = [
            let
                cellList = map (testBit i) [2, 1, 0]
                cellTripple = let [c1, c2, c3] = cellList in (c1, c2, c3)
            in
            (concatMap showCell cellList) ++ " -> " ++
            showCell (rule cellTripple) |
            i <- [0..7] :: [Int]
            ]

printSim = putStr . showSim

runSim _ 0 = return ()
runSim sim n = do
    putStr $ showSim sim
    runSim (stepSim sim) (n-1)

singleSeedRun n = runSim (fromNumber n, startState) numSteps
    where
        numSteps = 40
        numCs = 40
        startState = [i == div numCs 2 | i <- [0..numCs]]



delay = threadDelay . (1000*)