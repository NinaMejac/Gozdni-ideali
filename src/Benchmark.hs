-- | Benchmark is a module for time and space measurements
module Benchmark where

import ForestIdeals.Tree
import ForestIdeals.Advanced
import ForestIdeals.Simple

import Data.Time
import Criterion.Main
import Control.Monad.Trans

-- | Function doIt repeats function f n times
doIt f 0 = return ()
doIt f n = do
	f
	doIt f (n-1)

forest :: Forest
forest = [Tree 0 [Tree 1 []], Tree 2 [Tree 3 [], Tree 4 []]]

-- | Function simpleForestIdeals generates all ideals of a forest with Simple module
simpleForestIdeals :: [Tree] -> IO ()
simpleForestIdeals forest = do
	let
		coloring = create (createLength forest)
		state = [coloring]
		allStates = loop_forest drawIdeal forest state
	putStrLn $ unlines $ printIdeal forest allStates

-- | Function advancedForestIdeals generates all ideals of a forest with Advanced module
advancedForestIdeals :: Forest -> IO ()
advancedForestIdeals forest = do
	let
		allStates = loop_forest_ref forest
	putStrLn $ unlines $ printIdeal forest allStates

-- | Main generates space and time measurements.	
main = defaultMain [
  bgroup "ideals" [ bench "brez reference"  $ whnf simpleBenchmark forest
                  , bench "z referenco"  $ whnf advancedBenchmark forest
                  ]
  ]

-- | Functions stopWatchSimple and stopWatchAdvanced make measurements based on Simple and Advanced module
stopWatchSimple = stopwatchResult (simpleForestIdeals forest) 100
stopWatchAdvanced = stopwatchResult (advancedForestIdeals forest) 100

-- | Function simpleBenchmark generates all ideales using Simple module.
simpleBenchmark :: [Tree] -> [String]		
simpleBenchmark forest = do
	let
		coloring = create (createLength forest)
		state = [coloring]
		allStates = loop_forest drawIdeal forest state
	printIdeal forest allStates

-- | Function advancedBenchmark generates all ideales using Advanced module.
advancedBenchmark :: [Tree] -> [String]		
advancedBenchmark forest = printIdealAdvanced forest (loop_forest_ref forest)

-- | Function stopwatch generates measurements.
stopwatch :: MonadIO m => m a -> m NominalDiffTime
stopwatch computation = do
	start <- liftIO $ getCurrentTime
	_ <- computation
	end <- liftIO $ getCurrentTime
	return (end `diffUTCTime` start)

-- | Function stopwatchResult generates measurements.
stopwatchResult f n =
	stopwatch (doIt f n)
