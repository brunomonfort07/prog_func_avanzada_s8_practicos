-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module FoldExercises where
import Data.Foldable

{- PRIMER EJERCICIO:
4. In a similar manner, show how the following type of binary trees with data
in their nodes can be made into a foldable and traversable type:
data Tree a = Leaf | Node (Tree a) a (Tree a)
deriving Show
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show
    
instance Foldable Tree where
  fold Leaf = mempty
  -- Plegar los subárboles izquierdo y derecho, e incluir el nodo a en el resultado.
  fold (Node l a r) = fold l `mappend` a `mappend` fold r 

  foldMap f Leaf = mempty 
  -- Plegar los subárboles izquierdo y derecho, e incluir el nodo a en el resultado.
  foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

  foldr f b Leaf = b
  foldr f b (Node l a r) = foldr f (f a (foldr f b r)) l

{-foldr - Qué está pasando:
Se hace el foldr del subárbol derecho con b, obteniendo rightResult.
Se aplica f al valor actual a y rightResult, dando midResult.
Se hace el foldr del subárbol izquierdo con midResult.
→ Esto es exactamente el comportamiento esperado de foldr:
recorrer de derecha a izquierda e ir aplicando f.-}

  foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl f b Leaf = b
  foldl f b (Node l a r) = foldl f (f (foldl f b l) a) r

{-foldl - 
Primero aplica foldl al subárbol izquierdo con el acumulador b, obteniendo b'.
Luego aplica f b' a, usando el valor del nodo actual, obteniendo b''.
Finalmente, aplica foldl al subárbol derecho con b''.
-}

{-traverse = Aplica una función que devuelve un valor con efectos a
  cada elemento de la estructura.
  Devuelve una estructura dentro del contexto del efecto.-}

{- En el libro hay un Functor Tree con el a en la Leaf -}
instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Traversable Tree where
  traverse g Leaf = undefined
  traverse g (Node l a r) = undefined


{- PRIMER EJERCICIO:
5. Using foldMap, define a generic version of the higher-order function filter
on lists that can be used with any foldable type:
-}


