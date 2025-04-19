{-
Seccion 16.10 (pag 629) Ej 4 y 8

16.10 Intermission: Exercises
 Implement Functor instances for the following datatypes. 
-}

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

{- 
data Trivial = Trivial

No es posible implementar un Functor para Trivial porque es una estructura 
sin argumentos/parámetros de tipo en su constructor. Por lo tanto, no cumple 
con la definición que define y requiere un constructor unario (f a).
-}

{-
Seccion 16.11 (pag 634) Short Exercise, Dar el functor para Sum y responder la parte 2.

1. Write a Functor instance for a datatype identical to Either. We’ll
use our own datatype because Either also already has a Functor
instance.

Your hint for this one is that you’re writing the following function.
 
applyIfSecond :: (a-> b) -> (Sum e) a -> (Sum e) b
applyIfSecond = undefined
-}

data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second a) = Second (f a)
 
 {-
 RESPONDER:
 2. Why is a Functor instance that applies the function only to First, 
 Either’s Left, impossible? We covered this earlier.

 RESPUESTA:
 Porque los primeros parámetros son parte de la estructura funcional de un Functor, el "f".
 Functor tiene el tipo * -> *, y como tal solo actúa sobre el último parámetro.

 TODO SEGUIR VIENDO...
 -}

{-
Seccion 16.7 (pag 645) Implementar los siguientes functores:

Ej 5 LiftItOut
Ej 10 GoatLord
Ej 11TalkToMe
-}
--LiftItOut es un envoltorio (wrapper) de otro tipo f, que ya es un functor por sí mismo.
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap g (LiftItOut a) = LiftItOut (fmap g a)

data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)


instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print t a) = Print t (f a)
    fmap f (Read ta) = Read (fmap f ta)

--TODO PROBAR PROPS, CON QUICKCHECK O ALGO SIMILAR