module PrepareTree (prepareTree) where

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
parseNode n = return (Tree n [])

-- parse string
parseString :: Parser (Tree Char)
parseString = do
	n <- noneOf "[]"
	((parseSubforest n) <|> (parseNode n))
	
repairTree [Tree a []] stevec = Tree stevec []
repairTree [Tree a forest] stevec = Tree stevec [repairTree forest (stevec+1)]

prepareTree = do
	tree <- getLine
	parseTest parseString tree
	