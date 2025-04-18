{-
Seccion 16.10 (pag 629) Ej 4 y 8

16.10 Intermission: Exercises
 Implement Functor instances for the following datatypes. 
-}

data Three a b c = Three a b c

data Trivial = Trivial

{-
Seccion 16.11 (pag 634) Short Exercise, Dar el functor para Sum y responder la parte 2.

 1. Write a Functor instance for a datatype identical to Either. We’ll
 use our own datatype because Either also already has a Functor
 instance.
 -}
 data Sum a b =
 First a
 | Second b
 deriving (Eq, Show)
 instance Functor (Sum a) where
 fmap = undefined

 {-
 Your hint for this one is that you’re writing the following function.
 -}
 applyIfSecond :: (a-> b)-> (Sum e) a-> (Sum e) b
 
 {-RESPONDER:
 2. Why is a Functor instance that applies the function only to First, 
 Either’s Left, impossible? We covered this earlier
 -}

{-
Seccion 16.7 (pag 645) Implementar los siguientes functores:

Ej 5 LiftItOut
Ej 10 GoatLord
Ej 11TalkToMe-}
data LiftItOut f a = LiftItOut (f a)

data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

data TalkToMe a =
    Halt
    | Print String a
    | Read (String-> a)