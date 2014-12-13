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

import Data.STRef    (STRef, newSTRef, readSTRef, modifySTRef)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)

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

data Tree = Tree Int Forest deriving (Show, Read)
type Forest = [Tree]

-- Rocna definicija dreves iz clanka
tree1 = Tree 0 [Tree 1 []]
tree2 = Tree 2 [Tree 3 [], Tree 4 []]

-- Drevesi zdruzimo v gozd
forest = [tree1, tree2]

update node color coloring =
	take node coloring ++ [color] ++ drop (node + 1) coloring

resiNalogo = do
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
				traceShowM y
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
					modifyMyRef coloring (++ (update rootLabel 1 tmpColoring1)) 
					tmpColoring2 <- readMyRef coloring
					modifyMyRef coloring (\ tmpColoring2 -> drop (length tmpColoring1) tmpColoring2)
					enum_forest k subForest
			else
				do
					enum_forest k subForest
					a <- readMyRef coloring
					modifyMyRef coloring (++ (update rootLabel 0 a))
					b <- readMyRef coloring
					modifyMyRef coloring (\ b -> drop (length a) b)
					k ()
	
	enum_forest k forest

-- zagon
-- runST $ resiNalogo
