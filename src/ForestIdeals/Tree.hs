module ForestIdeals.Tree (Tree (..), Forest) where

-- kreiramo strukturo
data Tree = Tree Int Forest deriving (Show, Read)
type Forest = [Tree]