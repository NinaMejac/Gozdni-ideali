module Advanced (advancedForestIdealsA, Tree (..), Forest, advancedBenchmark) where

import Data.STRef
import Control.Monad.ST

-- kreiramo strukturo
data Tree = Tree Int Forest deriving (Show, Read)
type Forest = [Tree]


-- definicija novega tipa za shranjevanje reference
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

-- funkcija, ki vrne novo barvanje
updateRef node color coloring =
	take node coloring ++ [color] ++ drop (node + 1) coloring
	
loopForestRef forest = do
	allColors <- newMyRef [[0,0,0,0,0]]
	coloring <- newMyRef [0,0,0,0,0]
	
	let
		-- funkcija, ki vrne barvo zelenega vozlisca
		get node = do
			nodeValue <- newMyRef 0
			tmpColoring <- readMyRef coloring
			modifyMyRef nodeValue (+ (tmpColoring !! node))
			readMyRef nodeValue
			
		k () =
			do
				y <- readMyRef coloring
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

loop_forest_ref :: Forest -> [[Int]]
loop_forest_ref forest = 
	runST $ loopForestRef forest

-- pomozna funkcija za izris
izrisiIdeal forest (x:[]) =
	drawForest forest x
izrisiIdeal forest (x:xs) =
	(drawForest forest x) ++ ["\n\n\n---\n\n\n---"] ++ (izrisiIdeal forest xs)	

drawForest forest state =
	map f forest
		where f = drawTree2 state

drawTree tree state = unlines (draw tree state)
drawTree2 state tree = drawTree tree state
		
draw (Tree x ts0) state = 
  let vozlisce = (state !! x) in
  show vozlisce : drawSubTrees ts0
  where drawSubTrees [] = []
	drawSubTrees [t] =
		"|" : shift "`- " "   " (draw t state)
	drawSubTrees (t:ts) =
		"|" : shift "+- " "|  " (draw t state) ++ drawSubTrees ts

	shift first other = zipWith (++) (first : repeat other)

advancedForestIdealsA forest = do
	let
		allStates = loop_forest_ref forest
	putStrLn $ unlines $ izrisiIdeal forest allStates
	
advancedForestIdeals = do
	gozd <- getLine
	let
		forest = read gozd :: Forest
		allStates = loop_forest_ref forest
	putStrLn $ unlines $ izrisiIdeal forest allStates
	
advancedBenchmark forest = izrisiIdeal forest (loop_forest_ref forest)