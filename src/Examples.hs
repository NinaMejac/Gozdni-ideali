module Examples where

import ForestIdeals.Tree
import ForestIdeals.Advanced
import ForestIdeals.Simple

forest :: Forest
forest = [Tree 0 [Tree 1 []], Tree 2 [Tree 3 [], Tree 4 []]]

simpleForestIdeals :: [Tree] -> IO ()
simpleForestIdeals forest = do
	let
		coloring = create (createLength forest)
		state = [coloring]
		allStates = loop_forest drawIdeal forest state
	putStrLn $ unlines $ printIdeal forest allStates

advancedForestIdeals :: Forest -> IO ()
advancedForestIdeals forest = do
	let
		allStates = loop_forest_ref forest
	putStrLn $ unlines $ printIdeal forest allStates
	