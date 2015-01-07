-- | With module ForestIdeals.Simple we can generate all ideals of a given forest
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
-- | In the first coloring are all the nodes white (0).
create :: [Int] -> [Int]
create [] = []
create (h:t) = (take h(repeat (0 :: Int))) ++ (create t)

-- | Function drawForest calls a function for drawing trees for every tree in a forest.
drawForest :: Show a => [Tree] -> [a] -> [String]
drawForest forest state =
	map f forest
		where f = drawTree2 state

-- | Function drawTree is a auxiliary function for drawing trees.
drawTree :: Show a => Tree -> [a] -> String
drawTree tree state = unlines (draw tree state)

-- | Function drawTree2 is a auxiliary function for drawing trees.
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
	
-- funkcija vrne barvo node-a
get :: Int -> [[a]] -> a
get node allStates =
    let state = last allStates in
		state !! node
		
-- funkcija posodbi barvo node-a na color
update :: Int -> a -> [[a]] -> [[a]]
update node color allStates =
	let state = last allStates in
		allStates ++ [take node state ++ [color] ++ drop (node + 1) state]

drawIdeal :: t -> t		
drawIdeal allStates =
	allStates

-- loop_forest se sprehodi skozi gozd
-- ce je gozd prazen vrne stanje
-- sicer pa poskusi prebarvati preostanek gozda
loop_forest :: (Num a, Eq a) => ([[a]] -> [[a]]) -> Forest -> [[a]] -> [[a]]
loop_forest k forest state = case forest of
	[] -> k state
	tree : forest1 -> loop_tree (loop_forest k forest1) tree state

-- loop_tree preveri, kaksna je barva prvega naslednjega vozlisca
-- ce je belo, ga pobarva in nadaljuje z iskanjem nadaljnih resitev
-- ce je crno, nadaljuje iskanje na podrevesu in ga nato prebarva v belo
loop_tree :: (Num a, Eq a) => ([[a]] -> [[a]]) -> Tree -> [[a]] -> [[a]]
loop_tree k (Tree rootLabel subforest) state = 
	if get rootLabel state == 0 then
		let newState = update rootLabel 1 (k state) in 
			 loop_forest k subforest newState
	else
		k $ update rootLabel 0 $ loop_forest k subforest state
		
-- pomozna funkcija za izris
printIdeal :: Show a => [Tree] -> [[a]] -> [String]
printIdeal forest (x:[]) =
	drawForest forest x
printIdeal forest (x:xs) =
	(drawForest forest x) ++ ["\n\n\n---\n\n\n---"] ++ (printIdeal forest xs)	
