module ForestIdeals.PrepareTree (prepareTree) where

import Text.ParserCombinators.Parsec

-- tree structure
data Tree a = Tree a (Forest a ) deriving (Show, Read)
type Forest a = [Tree a]

-- parse node with subforest
parseSubforest n = do
	char '['
	f <- many parseString
	char ']'
	return (Tree n f)

-- parse node without subforest
parseNode :: Monad m => a -> m (Tree a)
parseNode n = return (Tree n [])

-- parse string
parseString :: Parser (Tree Char)
parseString = do
	n <- noneOf "[]"
	((parseSubforest n) <|> (parseNode n))
	
repairTree :: Num a => [Tree t] -> a -> Tree a
repairTree [Tree a []] stevec = Tree stevec []
repairTree [Tree a forest] stevec = Tree stevec [repairTree forest (stevec+1)]

prepareTree :: IO ()
prepareTree = do
	tree <- getLine
	parseTest parseString tree
	
