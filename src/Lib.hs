{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Data.List
import Data.Bits
import Control.Concurrent

type Cell = Bool
type Rule = (Cell, Cell, Cell) -> Cell
type SimulationState = [Cell]
type Simulation = (Rule, SimulationState)

-- | Forwards a simulation by a single step
stepSim :: Simulation -> Simulation
stepSim (rule, (c:cs)) = (rule, c : runKeepingTail (c:cs))
    where
        runKeepingTail [c1, c2] = [c2]
        runKeepingTail (c1:c2:c3:cs) = rule (c1, c2, c3) : runKeepingTail (c2:c3:cs)

-- | Creates a Rule from a number - rule from video: 145
ruleFromNumber :: Integer -> Rule
ruleFromNumber n =  testBit n . cellTrippleToNumber
    where
        cellTrippleToNumber = sum . zipWith (*) [4, 2, 1] . cellTrippleTo01        
        cellTrippleTo01 (c1, c2, c3) = map fromEnum [c1, c2, c3]

-- | shows the current state of a simulation
showSim :: Simulation -> String
showSim (_, cs) = showSimState cs ++ "\n"

-- | shows a simulation state
showSimState :: SimulationState -> String
showSimState = concatMap showCell

-- | shows a single cell in unicode
showCell :: Cell -> String
showCell c = if c then "🟥" else "🟨"

-- | shows the entire mapping of a single Rule in unicode
showRule :: Rule -> String
showRule rule = intercalate "\n" mappings ++ "\n"
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

-- | executes a simulation for n steps, printing its state in every iteration
runSim :: Simulation -> Integer -> IO ()
runSim _ 0 = return ()
runSim sim n = do
    putStr $ showSim sim
    runSim (stepSim sim) (n-1)