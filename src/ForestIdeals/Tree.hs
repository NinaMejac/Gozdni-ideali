-- | Module ForestIdeals.Tree is a module that generates forest or tree
module ForestIdeals.Tree (Tree (..), Forest) where

-- | A forest is a graph that is compound of trees.
-- 
--   A forest is represented as a list of trees.
--
--   Nodes of a tree are integers. They start with 0 and are increasing.

data Tree = Tree Int Forest deriving (Show, Read)
type Forest = [Tree]
