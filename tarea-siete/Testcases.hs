takeTreeList :: Int -> Tree a -> [Tree a]
takeTreeList 0 _ = []
takeTreeList _ Leaf = []
takeTreeList n (Node l x r) = takeTreeList (n-1) l ++ [Node Leaf x Leaf] ++ takeTreeList (n-1) r


-- Repetición infinita con repeatTree
test1 = take 3 $ map (\(Node _ x _) -> x) $ takeTreeList 3 (repeatTree 'A') 
-- Esperado: "AAA" (se extrae el valor del nodo raíz a lo largo del árbol)

-- TakeTree sobre un árbol finito
test2 = takeTree 2 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 
-- Esperado: Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- TakeTree cortando a profundidad 1
test3 = takeTree 1 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 
-- Esperado: Node Leaf 2 Leaf

-- ReplicateTree crea un árbol de profundidad n con el mismo valor
test4 = replicateTree 0 'Z' 
-- Esperado: Leaf

test5 = replicateTree 1 'Z' 
-- Esperado: Node Leaf 'Z' Leaf

test6 = replicateTree 2 'Z' 
-- Esperado: Node (Node Leaf 'Z' Leaf) 'Z' (Node Leaf 'Z' Leaf)

-- Comparación con árboles creados manualmente
expected6 = Node (Node Leaf 'Z' Leaf) 'Z' (Node Leaf 'Z' Leaf)
test6Ok = replicateTree 2 'Z' == expected6 
-- Esperado: True
