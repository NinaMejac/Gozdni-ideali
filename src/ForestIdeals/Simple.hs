module ForestIdeals.Simple (createLength, create, loop_forest, drawIdeal, printIdeal) where

import ForestIdeals.Tree

-- drevesu priredi seznam, katerega elementi so vizlišča drevesa
flatten :: Tree -> [Int]
flatten t = squish t []
  where squish (Tree x ts) xs = x:Prelude.foldr squish xs ts
  
-- createLenght poisce dolzino drevesa
createLength :: [Tree] -> [Int]
createLength [] = []
createLength (f:fx) = let
	treeLength = length (flatten f)
	in [treeLength] ++ createLength fx
	
-- create kreira seznam dolzine gozda, kjer so vsa vozlisca bele barve
-- i-ti element seznama predstavlja i-to vozlisce (rootLabel = i-to vozlisce)
create :: [Int] -> [Int]
create [] = []
create (h:t) = (take h(repeat (0 :: Int))) ++ (create t)

drawForest :: Show a => [Tree] -> [a] -> [String]
drawForest forest state =
	map f forest
		where f = drawTree2 state

drawTree :: Show a => Tree -> [a] -> String
drawTree tree state = unlines (draw tree state)
drawTree2 state tree = drawTree tree state

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