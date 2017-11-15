data SearchTree = Node SearchTree Int SearchTree | Leaf Int deriving Show

--balanced :: SearchTree -> Boolean
--balanced tree




-- Recursively find depth of subtree
checknode :: SearchTree -> Int -> Int 
checknode (Node l _ _ _) n = 0 
