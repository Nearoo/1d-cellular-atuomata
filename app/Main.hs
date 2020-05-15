module Main where

import Lib

-- | helper function: runs simulation with rule "ruleno" for n steps
runSimNo ruleno n = runSim (ruleFromNumber ruleno, startState) n
    where
        numCs = 40
        startState = [i == div numCs 2 | i <- [0..numCs]]

main = runSimNo 150 40 -- good ones: 150, 110, 165, 167