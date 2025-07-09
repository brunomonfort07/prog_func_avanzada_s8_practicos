module Laboratorio6 where

import Data.Monoid
import Data.Foldable
import Data.Traversable

-- 4 

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
  fold Leaf = mempty
  fold (Node l a r) = fold l <> a <> fold r

  foldMap _ Leaf = mempty
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

  foldr _ v Leaf = v
  foldr f v (Node l a r) = foldr f (f a (foldr f v r)) l

  foldl _ v Leaf = v
  foldl f v (Node l b r) = foldl f (f (foldl f v l) b) r

instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse g (Node l a r) = Node <$> traverse g l <*> g a <*> traverse g r


-- 5

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\x -> if f x then [x] else mempty)
