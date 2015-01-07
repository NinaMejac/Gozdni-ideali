module ForestIdeals.PrepareTree (prepareTree) where

import ForestIdeals.Tree
import Text.ParserCombinators.Parsec

-- | parse node with subforest
parseSubforest n = do
	char '['
	f <- many parseString
	char ']'
	return (Tree n f)

-- | parse node without subforest
parseNode :: Monad m => a -> m (Tree a)
parseNode n = return (Tree n [])

-- | parse string
parseString :: Parser (Tree Char)
parseString = do
	n <- noneOf "[]"
	((parseSubforest n) <|> (parseNode n))

-- | prepare tree
--
-- >>> prepareTree "2[34]"
-- Tree 2 [Tree 3 [],Tree 4 []]
prepareTree :: IO ()
prepareTree = do
	tree <- getLine
	parseTest parseString tree