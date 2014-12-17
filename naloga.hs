-- Vse uporabne in neuporabne knjiznice
import Data.List
import Data.Maybe
import Data.Char
import Data.String
import Debug.Trace
import System.IO
import System.IO.Error
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad.ST
import Control.Monad.Trans
import Data.STRef
import Control.Monad
import Data.Array.ST
import Criterion.Main
import Control.Monad.Trans
import Data.Time

-- kreiramo strukturo
data Tree = Tree Int Forest deriving (Show, Read)
type Forest = [Tree]

-- Rocna definicija dreves iz clanka
tree1 = Tree 0 [Tree 1 []]
tree2 = Tree 2 [Tree 3 [], Tree 4 []]

-- Drevesi zdruzimo v gozd
forest = [tree1, tree2]

-- | The elements of a tree in pre-order.
-- flatten :: Tree a -> [a]
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

getNodeFromState :: Int -> [Int] -> Int
getNodeFromState node state =
	state !! node

izrisiIdeal (x:[]) =
	drawForest forest x

izrisiIdeal (x:xs) =
	(drawForest forest x) ++ ["\n\n\n---\n\n\n---"] ++ (izrisiIdeal xs)

-- 1. naloga

-- na zacetku ustvarimo prazen coloring
coloring = create (createLength forest)

-- funkcija vrne barvo node-a
-- get :: Int -> [Int] -> Int
get node allStates =
    let state = last allStates in
		state !! node
		
-- funkcija posodbi barvo node-a na color
update node color allStates =
	let state = last allStates in
		allStates ++ [take node state ++ [color] ++ drop (node + 1) state]
		
-- v zacetnem stanju "state" so vsa vozlisca bela
--state :: [[Int]]
state = [coloring]
	
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

-- 2. naloga

data MyRef s a
	= MySTRef (STRef s a)
	| MyVal a

instance Num a => Num (MyRef s a) where
	fromInteger = MyVal . fromInteger

newMyRef :: a -> ST s (MyRef s a)
newMyRef x = do
	ref <- newSTRef x
	return (MySTRef ref)

readMyRef :: MyRef s a -> ST s a
readMyRef (MySTRef x) = readSTRef x
readMyRef (MyVal x) = return x

modifyMyRef (MySTRef x) f = writeSTRef x . f =<< readSTRef x

updateRef node color coloring =
	take node coloring ++ [color] ++ drop (node + 1) coloring

resiNalogo = do
	allColors <- newMyRef [[0,0,0,0,0]]
	coloring <- newMyRef [0,0,0,0,0]
	
	let
		get node = do
			nodeValue <- newMyRef 0
			tmpColoring <- readMyRef coloring
			modifyMyRef nodeValue (+ (tmpColoring !! node))
			readMyRef nodeValue
			
		k () =
			do
				y <- readMyRef coloring
				-- traceShowM y
				return ()
		
		enum_forest k forest = 
			case forest of
				[] -> k ()
				t : f -> enum_tree (\() -> enum_forest k f) t
				
		enum_tree k (Tree rootLabel subForest) = do
			compare <- get rootLabel
			if compare == 0 then
				do
					k ()
					tmpColoring1 <- readMyRef coloring
					modifyMyRef coloring (++ (updateRef rootLabel 1 tmpColoring1)) 
					tmpColoring2 <- readMyRef coloring
					modifyMyRef coloring (\ tmpColoring2 -> drop (length tmpColoring1) tmpColoring2)
					lastColoring <- readMyRef coloring
					modifyMyRef allColors (++ [lastColoring])
					enum_forest k subForest
			else
				do
					enum_forest k subForest
					a <- readMyRef coloring
					modifyMyRef coloring (++ (updateRef rootLabel 0 a))
					b <- readMyRef coloring
					modifyMyRef coloring (\ b -> drop (length a) b)
					lastColoring <- readMyRef coloring
					modifyMyRef allColors (++ [lastColoring])
					k ()
	
	enum_forest k forest
	readMyRef allColors
	
mojaNaloga :: [[Int]]
mojaNaloga = 
	runST $ resiNalogo

brezReference = putStrLn $ unlines $ izrisiIdeal allStates
zReferenco = putStrLn $ unlines $ izrisiIdeal mojaNaloga

meritev funkcija 0 = return ()
meritev funkcija n =
 do
  funkcija
  meritev funkcija (n-1)

-- benchmarking ...
{--main = defaultMain [
  bgroup "ideals" [ bench "brez reference"  $ whnf izrisiIdeal allStates
                  , bench "z referenco"  $ whnf izrisiIdeal mojaNaloga
                  ]
  ]
--}

-- | Run a stop-watch at the start and end of a computation.
stopwatch :: MonadIO m => m a -> m NominalDiffTime
stopwatch computation = do
  start <- liftIO $ getCurrentTime
  _ <- computation
  end <- liftIO $ getCurrentTime
  return (end `diffUTCTime` start)


izmeri funkcija steviloPonovitev =
	stopwatch (meritev funkcija steviloPonovitev)

--putStrLn $ unlines $ izrisiIdeal allStates