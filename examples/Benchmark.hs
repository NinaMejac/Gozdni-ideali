module Benchmark (stopwatchResult) where

import ForestIdeals.Tree
import ForestIdeals.Advanced
import ForestIdeals.Simple

import Data.Time
import Criterion.Main
import Control.Monad.Trans

-- do f n-times
doIt f 0 = return ()
doIt f n = do
	f
	doIt f (n-1)

-- benchmarking ...

simpleForest :: Forest
simpleForest = [Tree 0 [Tree 1 []], Tree 2 [Tree 3 [], Tree 4 []]]

simpleForestIdeals :: [Tree] -> IO ()
simpleForestIdeals forest = do
	let
		coloring = create (createLength forest)
		state = [coloring]
		allStates = loop_forest drawIdeal forest state
	putStrLn $ unlines $ printIdeal forest allStates

advancedForest :: Forest
advancedForest = [Tree 0 [Tree 1 []], Tree 2 [Tree 3 [], Tree 4 []]]

advancedForestIdeals :: Forest -> IO ()
advancedForestIdeals forest = do
	let
		allStates = loop_forest_ref forest
	putStrLn $ unlines $ printIdeal forest allStates
	
main = defaultMain [
  bgroup "ideals" [ bench "brez reference"  $ whnf simpleBenchmark simpleForest
                  , bench "z referenco"  $ whnf advancedBenchmark advancedForest
                  ]
  ]

stopWatchSimple = stopwatchResult (simpleForestIdeals simpleForest) 100
stopWatchAdvanced = stopwatchResult (advancedForestIdeals advancedForest) 100

simpleBenchmark :: [Tree] -> [String]		
simpleBenchmark forest = do
	let
		coloring = create (createLength forest)
		state = [coloring]
		allStates = loop_forest drawIdeal forest state
	printIdeal forest allStates

advancedBenchmark :: [Tree] -> [String]		
advancedBenchmark forest = printIdealAdvanced forest (loop_forest_ref forest)

-- funkcija za izvajanje meritev
stopwatch :: MonadIO m => m a -> m NominalDiffTime
stopwatch computation = do
	start <- liftIO $ getCurrentTime
	_ <- computation
	end <- liftIO $ getCurrentTime
	return (end `diffUTCTime` start)

stopwatchResult f n =
	stopwatch (doIt f n)