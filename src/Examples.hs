module Examples where

import ForestIdeals.Tree
import ForestIdeals.Advanced
import ForestIdeals.Simple

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
	