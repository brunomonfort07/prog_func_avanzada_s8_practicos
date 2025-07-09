module Laboratorio3 where

-- 629 ej. 4
data Three a b c = Three a b c 
    deriving (Show, Eq)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)


--629 ej. 8

data Trivial = Trivial

-- Esto NO se puede hacer:
-- instance Functor Trivial where
-- Porque Trivial no es un tipo polimorfico, o sea no tiene una variable de tipo 'a' que pueda aplicarse la funcion mapeada.


--634 ej. "Short Exercise: Functor"

data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First x)  = First x
    fmap f (Second y) = Second (f y)

-- ej. 5
data LiftItOut f a = LiftItOut (f a)
    deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa ) = LiftItOut (fmap f fa)

-- ej. 10
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- ej. 11
data TalkToMe a = Halt | Print String a | Read (String -> a)


instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (f . g)


-- cap 17
-- Functor y Applicative para Validation

data Validation e a = 
      Failure e 
    | Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure = Success
    Success f <*> Success a = Success (f a)
    Failure e1 <*> Failure e2 = Failure (e1 <> e2)
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e

-- Ejercicio 4

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    Three a1 b1 f <*> Three a2 b2 x = Three (a1 <> a2) (b1 <> b2) (f x)

