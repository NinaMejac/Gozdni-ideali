import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import System.IO
import System.IO.Error
import System.Environment

data Tree = Tree Int Forest deriving (Show, Read)
type Forest = [Tree]

-- Rocna definicija dreves iz clanka
tree1 = Tree 0 [Tree 1 []]
tree2 = Tree 2 [Tree 3 [], Tree 4 []]

-- Drevesi zdruzimo v gozd
forest = [tree1, tree2]

-- | The elements of a tree in pre-order.
--flatten :: Tree a -> [a]
flatten t = squish t []
  where squish (Tree x ts) xs = x:Prelude.foldr squish xs ts

-- createLenght poisce dolzino drevesa
createLength [] = []
createLength (f:fx) = let
	treeLength = length (flatten f)
	in [treeLength] ++ createLength fx
	
-- create kreira seznam dolzine gozda, kjer so vsa vozlisca bele barve
-- i-ti element seznama predstavlja i-to vozlisce (rootLabel = i-to vozlisce)
create [] = []
create (h:t) = (take h(repeat (0 :: Int))) ++ (create t)

-- na zacetku ustvarimo prazen coloring
coloring = create (createLength forest)

-- funkcija vrne barvo node-a
-- get :: Int -> [Int] -> Int
get node allStates =
    let state = last allStates in
		state !! node
		
getNodeFromState :: Int -> [Int] -> Int
getNodeFromState node state =
	state !! node
	
-- funkcija posodbi barvo node-a na color
update node color allStates =
	let state = last allStates in
		allStates ++ [take node state ++ [color] ++ drop (node + 1) state]
		
-- v zacetnem stanju "state" so vsa vozlisca bela
--state :: [[Int]]
state = [coloring]

drawTree tree state = unlines (draw tree state)
drawTree2 state tree = drawTree tree state

drawForest forest state =
	map f forest
		where f = drawTree2 state
		
draw (Tree x ts0) state = 
  let vozlisce = getNodeFromState x state in
  show vozlisce : drawSubTrees ts0
  where drawSubTrees [] = []
	drawSubTrees [t] =
		"|" : shift "`- " "   " (draw t state)
	drawSubTrees (t:ts) =
		"|" : shift "+- " "|  " (draw t state) ++ drawSubTrees ts

	shift first other = zipWith (++) (first : repeat other)
	
drawIdeal allStates =
	allStates
	
-- loop_forest se sprehodi skozi gozd
-- ce je gozd prazen vrne stanje
-- sicer pa poskusi prebarvati preostanek gozda
loop_forest k forest state = case forest of
	[] -> k state
	tree : forest1 -> loop_tree (loop_forest k forest1) tree state

-- loop_tree preveri, kaksna je barva prvega naslednjega vozlisca
-- ce je belo, ga pobarva in nadaljuje z iskanjem nadaljnih resitev
-- ce je crno, nadaljuje iskanje na podrevesu in ga nato prebarva v belo
loop_tree k (Tree rootLabel subforest) state = 
	if get rootLabel state == 0 then
		let newState = update rootLabel 1 (k state) in 
			 loop_forest k subforest newState
	else
		k $ update rootLabel 0 $ loop_forest k subforest state

-- primer zagona programa (v konzoli)
-- loop_forest drawIdeal forest state

allStates = loop_forest drawIdeal forest state

izrisiIdeal (x:[]) =
	drawForest forest x

izrisiIdeal (x:xs) =
	(drawForest forest x) ++ ["\n\n\n---\n\n\n---"] ++ (izrisiIdeal xs)
	
--handler :: IOError -> IO()
--ha-ndler e = putStrLn "Gozd v napacni obliki!"
	
--main = izris `catch` handler

main = do
	putStrLn "Vnesite gozd: "
	gozd <- getLine
	putStrLn $ unlines $ izrisiIdeal (loop_forest drawIdeal (read gozd :: Forest) state)


--putStrLn $ unlines $ izrisiIdeal allStates
-- [Tree 0 [Tree 1 []], Tree 2 [Tree 3 [], Tree 4 []]]