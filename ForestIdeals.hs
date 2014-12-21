import PrepareTree
import Simple
import Advanced

simpleForest :: Simple.Forest
simpleForest = [Simple.Tree 0 [Simple.Tree 1 []], Simple.Tree 2 [Simple.Tree 3 [], Simple.Tree 4 []]]

advancedForest :: Advanced.Forest
advancedForest = [Advanced.Tree 0 [Advanced.Tree 1 []], Advanced.Tree 2 [Advanced.Tree 3 [], Advanced.Tree 4 []]]

forestIdealsSimple = simpleForestIdealsA simpleForest
forestIdealsAdvanced = advancedForestIdealsA advancedForest