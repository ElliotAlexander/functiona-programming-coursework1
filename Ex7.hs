data SearchTree = Node SearchTree Int SearchTree | Leaf Int deriving Show

--balanced :: SearchTree -> Int -> Boolean
--balanced tree current_depth
--    | isNode tree == True = 
--    | otherwise = 


checknode :: SearchTree -> Int -> Bool
checknode (Node _ _ _) tree x = False
checknode (Leaf _) leaf x = True


isNode :: SearchTree -> Bool
isNode (Node _ _ _ ) = True
isNode _ = False