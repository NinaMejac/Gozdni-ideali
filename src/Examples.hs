module Examples where

import ForestIdeals.Tree
import ForestIdeals.Advanced
import ForestIdeals.Simple

-- | create forest from article
forest :: Forest
forest = [Tree 0 [Tree 1 []], Tree 2 [Tree 3 [], Tree 4 []]]

-- | simpleForestIdeals generates all forest ideals of a given forest
--
-- >>> simpleForestIdeals forest
simpleForestIdeals :: [Tree] -> IO ()
simpleForestIdeals forest = do
	let
		coloring = create (createLength forest)
		state = [coloring]
		allStates = loop_forest drawIdeal forest state
	putStrLn $ unlines $ printIdeal forest allStates

-- | advancedForestIdeals generates all forest ideals of a given forest
--
-- >>> advancedForestIdeals forest
advancedForestIdeals :: Forest -> IO ()
advancedForestIdeals forest = do
	let
		allStates = loop_forest_ref forest
	putStrLn $ unlines $ printIdeal forest allStates
	