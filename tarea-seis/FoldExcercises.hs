-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module FoldExercises where

{- PRIMER EJERCICIO:
4. In a similar manner, show how the following type of binary trees with data
in their nodes can be made into a foldable and traversable type:
data Tree a = Leaf | Node (Tree a) a (Tree a)
deriving Show
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show




{- PRIMER EJERCICIO:
5.Using foldMap, define a generic version of the higher-order function filter
on lists that can be used with any foldable type:
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
-}

instance Foldable Tree where
--  TO DO...

instance Traversable Tree where
--  TO DO...
