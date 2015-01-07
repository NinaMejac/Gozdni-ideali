module Main where

import ForestIdeals.Simple
import ForestIdeals.Advanced
import ForestIdeals.Tree

-- | simpleForestIdeals generates all forest ideals of a given forest
--
-- >>> simpleForestIdeals
-- [Tree 0 [Tree 1 []], Tree 2 [Tree 3 [], Tree 4 []]]
simpleForestIdeals :: IO ()
simpleForestIdeals = do
	forestInput <- getLine
	let
		forest = read forestInput :: Forest
		coloring = create (createLength forest)
		state = [coloring]
		allStates = loop_forest drawIdeal forest state
	putStrLn $ unlines $ printIdeal forest allStates

-- | advancedForestIdeals generates all forest ideals of a given forest
--
-- >>> advancedForestIdeals
-- [Tree 0 [Tree 1 []], Tree 2 [Tree 3 [], Tree 4 []]]
advancedForestIdeals :: IO ()
advancedForestIdeals = do
	forestInput <- getLine
	let
		forest = read forestInput :: Forest
		allStates = loop_forest_ref forest
	putStrLn $ unlines $ printIdealAdvanced forest allStates