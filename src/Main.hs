module Main where

import ForestIdeals.Simple
import ForestIdeals.Advanced
import ForestIdeals.Tree

simpleForestIdeals :: IO ()
simpleForestIdeals = do
	forestInput <- getLine
	let
		forest = read forestInput :: Forest
		coloring = create (createLength forest)
		state = [coloring]
		allStates = loop_forest drawIdeal forest state
	putStrLn $ unlines $ printIdeal forest allStates


advancedForestIdeals :: IO ()
advancedForestIdeals = do
	forestInput <- getLine
	let
		forest = read forestInput :: Forest
		allStates = loop_forest_ref forest
	putStrLn $ unlines $ printIdealAdvanced forest allStates