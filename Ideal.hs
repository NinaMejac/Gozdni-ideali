import Data.Tree
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import System.IO

-- Rocna definicija dreves iz clanka
tree1 = Node { rootLabel = "0", subForest = [Node { rootLabel = "1", subForest = []}]}
tree2 = Node { rootLabel = "2", subForest = [Node { rootLabel = "3", subForest = []}, Node { rootLabel = "4", subForest = []}]}

-- Drevesi zdruzimo v gozd
forest = [tree1, tree2]

-- funkcija flattenForest gozd zlozi v seznam
-- flattenForest [] = []
-- flattenForest (f:fx) = flatten f ++ flattenForest fx

-- createLenght poisce dolzino drevesa
createLength [] = []
createLength (f:fx) = let
	treeLength = length (flatten f)
	in [treeLength] ++ createLength fx

-- create kreira seznam dolzine gozda, kjer so vsa vozlisca bele barve
-- i-ti element seznama predstavlja i-to vozlisce (rootLabel = i-to vozlisce)
create [] = []
create (h:t) = (take h(repeat 0)) ++ (create t)

-- na zacetku ustvarimo prazen coloring
coloring = create (createLength forest)

-- funkcija vrne barvo node-a (podamo ji seznam stanj)
get node allStates =
	let state = last allStates in
		state !! node

-- funkcija vrne barvo node-a (podamo ji samo eno stanje)
getNodeFromState :: Int -> [Int] -> Int
getNodeFromState node state =
	state !! node

-- funkcija posodbi barvo node-a na color in vrne seznam vseh stanj (z dodanim zadnjim, novim stanjem)
update node color allStates =
	let state = last allStates in
		allStates ++ [take node state ++ [color] ++ drop (node + 1) state]

-- v zacetnem stanju "state" so vsa vozlisca bela
state :: [[Int]]
state = [coloring]

-- Posodoboljena funkcija za izris drevesa (osnova drawTree)
drawTree1 tree state = unlines (draw1 tree state)

-- Enako kot drawTree1, le da obrnemo argumente
drawTree2 state tree = drawTree1 tree state

-- Posodobljena funkcija za izris gozda (osnova drawForest)
drawForest1 forest state =
	map f forest
		where f = drawTree2 state

-- Posodobljena funkcija draw
draw1 (Node x ts0) state = 
  let vozlisce = getNodeFromState (read x::Int) state in
  show vozlisce : drawSubTrees ts0
  where drawSubTrees [] = []
	drawSubTrees [t] =
		"|" : shift "`- " "   " (draw1 t state)
	drawSubTrees (t:ts) =
		"|" : shift "+- " "|  " (draw1 t state) ++ drawSubTrees ts

	shift first other = zipWith (++) (first : repeat other)

-- funkcija drawIdeal vrne vsa stanja
drawIdeal allStates =
	allStates

loop_forest k forest state = case forest of
	[] -> k state
	tree : forest1 -> loop_tree (loop_forest k forest1) tree state

-- loop_tree preveri, kaksna je barva prvega naslednjega vozlisca
-- ce je belo, ga pobarva in nadaljuje z iskanjem nadaljnih resitev
-- ce je crno, nadaljuje iskanje na podrevesu in ga nato prebarva v belo
loop_tree k (Node rootLabel subforest) state = 
	let i = read rootLabel::Int in
		if get i state == 0 then
			let newState = update i 1 (k state) in 
			   loop_forest k subforest newState
		else
			k $ update i 0 $ loop_forest k subforest state

ideal = loop_forest drawIdeal forest state

printIdeal (x:[]) =
	drawForest1 forest x

printIdeal (x:xs) =
	-- med posameznimi drevesi dodamo --- --- ---, da jih lahko locimo
	(drawForest1 forest x) ++ ["--- --- ---\n"] ++ (printIdeal xs)

-- primer zagona programa (v konzoli)
-- ideal
-- primer zagona programa (v konzoli) - izris vseh idealov
-- putStrLn $ unlines $ printIdeal ideal