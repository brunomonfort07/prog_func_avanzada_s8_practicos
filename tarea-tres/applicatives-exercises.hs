{-
Cap 17 Applicative

(Pag 692) Implementar Functor y Applicative para Validation
Seccion 17.9 (pag 694) Ej 4. data Three a b c = Three a b c
-}

data Validation err a = Failure err | Success a 
     deriving (Eq, Show)
    
instance Functor (Validation e) where
     fmap _ (Failure err) = Failure err
     fmap f (Success a) = Success (f a)


{-
class Functor f => Applicative f where
  pure  :: a -> f a --- pure toma un valor normal y lo mete en el contexto. pure "hola"   -- ["hola"]
  (<*>) :: f (a -> b) -> f a -> f b
-}
instance Functor (Validation e) => Applicative (Validation e) where
     pure err = Failure err
     (<*>) = undefined

{-

(<$>) :: Functor f => (a -> b) -> f a -> f b   -- fmap
(<*>) :: Applicative f => f (a -> b) -> f a -> f b   -- apply
The difference is the 𝑓 representing functorial structure that is on
the outside of our function in the second definition.

-}