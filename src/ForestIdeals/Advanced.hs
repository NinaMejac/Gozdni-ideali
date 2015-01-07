-- | With module ForestIdeals.Advanced we can generate all ideals of a given forest
module ForestIdeals.Advanced (loop_forest_ref, printIdealAdvanced) where

import Data.STRef
import Control.Monad.ST
import ForestIdeals.Tree

-- | Module Advanced generates all ideals of a forest. During computation all of them are in a list. List is stored by reference.

-- | New data type MyRef, which is compound of STRef value and pure value.
data MyRef s a
	= MySTRef (STRef s a)
	| MyVal a

-- | A Num instance for MyRef. 
instance Num a => Num (MyRef s a) where
	fromInteger = MyVal . fromInteger

-- | Function newMyRef generates new MyRef value.
newMyRef :: a -> ST s (MyRef s a)
newMyRef x = do
	ref <- newSTRef x
	return (MySTRef ref)

-- | Function readRef reads a value of MyRef.
readMyRef :: MyRef s a -> ST s a
readMyRef (MySTRef x) = readSTRef x
readMyRef (MyVal x) = return x

-- | Function modifyMyRef modifiyies a value of MyRef.
modifyMyRef :: MyRef s a -> (a -> a) -> ST s ()
modifyMyRef (MySTRef x) f = writeSTRef x . f =<< readSTRef x

-- | Function updateRef generates new coloring.
updateRef :: Int -> a -> [a] -> [a]
updateRef node color coloring =
	take node coloring ++ [color] ++ drop (node + 1) coloring

-- | Function loopForestRef generates all coloring of a given forest.
loopForestRef :: (Num a, Eq a) => Forest -> ST s [[a]]	
loopForestRef forest = do
	allColors <- newMyRef [[0,0,0,0,0]]
	coloring <- newMyRef [0,0,0,0,0]
	
	let
		-- Function get return a color of a node
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

-- | Function loop_forest_ref is an auxiliary function for generating all coloring of a forest.
loop_forest_ref :: Forest -> [[Int]]
loop_forest_ref forest = 
	runST $ loopForestRef forest

-- | Function printIdealAdvanced is an auxiliary function for drawing trees.
printIdealAdvanced :: Show a => [Tree] -> [[a]] -> [String]
printIdealAdvanced forest (x:[]) =
	drawForest forest x
printIdealAdvanced forest (x:xs) =
	(drawForest forest x) ++ ["\n\n\n---\n\n\n---"] ++ (printIdealAdvanced forest xs)

-- | Function drawForest calls a function for drawing trees for every tree in a forest.
drawForest :: Show a => [Tree] -> [a] -> [String]
drawForest forest state =
	map f forest
		where f = drawTree2 state

-- | Function drawTree is an auxiliary function for drawing trees.
drawTree :: Show a => Tree -> [a] -> String
drawTree tree state = unlines (draw tree state)

-- | Function drawTree is an auxiliary function for drawing trees.
drawTree2 :: Show a => [a] -> Tree -> String
drawTree2 state tree = drawTree tree state

-- | Function draw prints a tree. In the output the names of nodes are switched with its color.
draw :: Show a => Tree -> [a] -> [[Char]]			
draw (Tree x ts0) state = 
  let node = (state !! x) in
  show node : drawSubTrees ts0
  where drawSubTrees [] = []
	drawSubTrees [t] =
		"|" : shift "`- " "   " (draw t state)
	drawSubTrees (t:ts) =
		"|" : shift "+- " "|  " (draw t state) ++ drawSubTrees ts

	shift first other = zipWith (++) (first : repeat other)
