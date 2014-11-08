import Data.Tree
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace

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

-- funkcija vrne barvo node-a
get node allStates =
	let state = last allStates in
	state !! node
	
-- funkcija posodbi barvo node-a na color
update node color allStates =
	let state = last allStates in
    take node state ++ [color] ++ drop (node + 1) state

-- v zacetnem stanju "state" so vsa vozlisca bela
state = [coloring]

-- loop_forest se sprehodi skozi gozd
-- ce je gozd prazen vrne stanje
-- sicer pa poskusi prebarvati preostanek gozda
loop_forest forest state = case forest of
	[] -> state
	tree : forest1 -> loop_tree tree (loop_forest forest1 state)

-- loop_tree preveri, kaksna je barva prvega naslednjega vozlisca
-- ce je belo, ga pobarva in nadaljuje z iskanjem nadaljnih resitev
-- sicer - bug: se ne zgodi
loop_tree (Node rootLabel subforest) state =
	let i = read rootLabel::Int in
		if get i state == 0 then
			loop_forest subforest (state ++ [update i 1 state])
		else
			-- nikoli ne pridemo do sem?
			loop_forest subforest (state ++ [update i 0 state])

-- primer zagona programa (v konzoli)
-- *Main> loop_forest forest state

-- zeljene resitve za zgornji primer
-- [[0,0,0,0,0],[0,0,1,0,0],[0,0,1,0,1],[0,0,1,1,1],[0,0,1,1,0],[1,0,0,0,0],[1,0,1,0,0],[1,0,1,0,1],[1,0,1,1,1],[1,0,1,1,0],[1,1,0,0,0],[1,1,1,0,0],[1,1,1,0,1],[1,1,1,1,1],[1,1,1,1,0]]
-- trenutne resitve
-- [[0,0,0,0,0],[0,0,1,0,0],[0,0,1,0,1],[0,0,1,1,1],[1,0,1,1,1],[1,1,1,1,1]]