-- | Module Simple generates all ideals of a forest by collecting them in one list
module ForestIdeals.Simple (createLength, create, loop_forest, drawIdeal, printIdeal) where

import ForestIdeals.Tree

-- | Function flatten generates a list of nodes of a given tree
flatten :: Tree -> [Int]
flatten t = squish t []
  where squish (Tree x ts) xs = x:Prelude.foldr squish xs ts
  
-- | Function createLength generates a list of nodes of a forest flatten in a list
createLength :: [Tree] -> [Int]
createLength [] = []
createLength (f:fx) = let
	treeLength = length (flatten f)
	in [treeLength] ++ createLength fx
	
-- | Function create generates the first coloring of a forest. In a coloring list the ith element represents the ith node of a forest.
--
--   In the first coloring are all the nodes white (0).
create :: [Int] -> [Int]
create [] = []
create (h:t) = (take h(repeat (0 :: Int))) ++ (create t)

-- | Function drawForest calls a function for drawing trees for every tree in a forest.
drawForest :: Show a => [Tree] -> [a] -> [String]
drawForest forest state =
	map f forest
		where f = drawTree2 state

-- | Function drawTree is an auxiliary function for drawing trees.
drawTree :: Show a => Tree -> [a] -> String
drawTree tree state = unlines (draw tree state)

-- | Function drawTree2 is an auxiliary function for drawing trees.
drawTree2 :: Show a => [a] -> Tree -> String
drawTree2 state tree = drawTree tree state

-- | Function draw prints a tree. In the output the names of nodes are switched with its color.
draw :: Show a => Tree -> [a] -> [[Char]]
draw (Tree x ts0) state = 
  let vozlisce = (state !! x) in
  show vozlisce : drawSubTrees ts0
  where drawSubTrees [] = []
	drawSubTrees [t] =
		"|" : shift "`- " "   " (draw t state)
	drawSubTrees (t:ts) =
		"|" : shift "+- " "|  " (draw t state) ++ drawSubTrees ts

	shift first other = zipWith (++) (first : repeat other)
	
-- | Function get returns a color of the node in the last coloring of a forest.
get :: Int -> [[a]] -> a
get node allStates =
    let state = last allStates in
		state !! node
		
-- | Function update creates a new coloring of a forest.
--
--   The color of a node in the last coloring is changed to new color.
update :: Int -> a -> [[a]] -> [[a]]
update node color allStates =
	let state = last allStates in
		allStates ++ [take node state ++ [color] ++ drop (node + 1) state]

-- | Function drawIdeal is a function that returns argument itself.
drawIdeal :: t -> t
drawIdeal allStates =
	allStates

-- | Function loop_forest walks through the forest. If that forest is empty then it returns current state.
--
--   In all other cases the function tries to color the rest of the forest.
loop_forest :: (Num a, Eq a) => ([[a]] -> [[a]]) -> Forest -> [[a]] -> [[a]]
loop_forest k forest state = case forest of
	[] -> k state
	tree : forest1 -> loop_tree (loop_forest k forest1) tree state

-- | Function loop_tree checks the color of a root node.
--
--   If a color of a node is white (0), the function colors it and continues on subforest.
--
--   If a color of a node is black (1), the function continues on subforest and then it colors the node to white (0).
loop_tree :: (Num a, Eq a) => ([[a]] -> [[a]]) -> Tree -> [[a]] -> [[a]]
loop_tree k (Tree rootLabel subforest) state = 
	if get rootLabel state == 0 then
		let newState = update rootLabel 1 (k state) in 
			 loop_forest k subforest newState
	else
		k $ update rootLabel 0 $ loop_forest k subforest state
		
-- | Function printIdeal is an auxiliary function for drawing trees.
printIdeal :: Show a => [Tree] -> [[a]] -> [String]
printIdeal forest (x:[]) =
	drawForest forest x
printIdeal forest (x:xs) =
	(drawForest forest x) ++ ["\n\n\n---\n\n\n---"] ++ (printIdeal forest xs)