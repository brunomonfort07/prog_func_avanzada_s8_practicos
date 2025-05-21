-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module FoldExercises where
import Data.Foldable

{- PRIMER EJERCICIO:
4. In a similar manner, show how the following type of binary trees with data
in their nodes can be made into a foldable and traversable type:
data Tree a = Leaf | Node (Tree a)  (Tree a)
deriving Show
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Foldable Tree where
  fold :: Monoid m => Tree m -> m
  fold Leaf = mempty
  fold (Node l a r) = fold l `mappend` a `mappend` fold r

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b Leaf = b
  foldr f b (Node l a r) = foldr f (f a (foldr f b r)) l

  foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl f b Leaf = b
  foldl f b (Node l a r) = foldl f (f (foldl f b l) a) r

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf = pure Leaf
  traverse g (Node l a r) = pure Node <*> traverse g l <*> g a <*> traverse g r

{- SEGUNDO EJERCICIO:
5. Using foldMap, define a generic version of the higher-order function filter
on lists that can be used with any foldable type:
-}
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p ta = foldMap (\a -> if (p a) then [a] else []) ta