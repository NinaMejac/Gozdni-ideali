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
get node state =
	state !! node

-- funkcija posodbi barvo node-a na color
update node color state =
	take node state ++ [color] ++ drop (node + 1) state

-- v zacetnem stanju "state" so vsa vozlisca bela
state = coloring

-- loop_forest se sprehodi skozi gozd
-- ce je gozd prazen vrne stanje
-- sicer pa poskusi prebarvati preostanek gozda

drawIdeal state =
	-- s pomocjo traceShow izrisemo trenutno stanje gozda
	traceShow state
	-- vrnemo state
	state

loop_forest k forest state = case forest of
	[] -> k state
	tree : forest1 -> loop_tree (loop_forest k forest1) tree state

-- loop_tree preveri, kaksna je barva prvega naslednjega vozlisca
-- ce je belo, ga pobarva in nadaljuje z iskanjem nadaljnih resitev
-- ce je crno, nadaljuje iskanje na podrevesu in ga nato prebarva v belo
loop_tree k (Node rootLabel subforest) state =
	let i = read rootLabel::Int in
		if get i state == 0 then
			loop_forest k subforest (update i 1 (k state))
		else
			k (update i 0 (loop_forest k subforest state))

-- primer zagona programa (v konzoli)
-- loop_forest drawIdeal forest state