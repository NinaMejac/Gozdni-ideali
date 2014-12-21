module Benchmark (stopwatchResult) where

import Data.Time
import Simple
import Advanced
import Criterion.Main
import Control.Monad.Trans
import Data.Time


-- do f n-times
doIt f 0 = return ()
doIt f n = do
	f
	doIt f (n-1)

-- benchmarking ...

simpleForest :: Simple.Forest
simpleForest = [Simple.Tree 0 [Simple.Tree 1 []], Simple.Tree 2 [Simple.Tree 3 [], Simple.Tree 4 []]]

advancedForest :: Advanced.Forest
advancedForest = [Advanced.Tree 0 [Advanced.Tree 1 []], Advanced.Tree 2 [Advanced.Tree 3 [], Advanced.Tree 4 []]]

main = defaultMain [
  bgroup "ideals" [ bench "brez reference"  $ whnf simpleBenchmark simpleForest
                  , bench "z referenco"  $ whnf advancedBenchmark advancedForest
                  ]
  ]

-- funkcija za izvajanje meritev
stopwatch :: MonadIO m => m a -> m NominalDiffTime
stopwatch computation = do
	start <- liftIO $ getCurrentTime
	_ <- computation
	end <- liftIO $ getCurrentTime
	return (end `diffUTCTime` start)

stopwatchResult f n =
	stopwatch (doIt f n)


stopWatchSimple = stopwatchResult (simpleForestIdealsA simpleForest) 100
stopWatchAdvanced = stopwatchResult (Advanced.advancedForestIdealsA advancedForest) 100
