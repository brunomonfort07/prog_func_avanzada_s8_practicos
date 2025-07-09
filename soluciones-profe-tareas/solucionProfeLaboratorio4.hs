module Laboratorio4 where

-- Ej 4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)
    
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

-- Ej 8
newtype ST a = S (State -> (a, State))
type State = Int

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do
        x <- st
        return (g x)

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))
    
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do
        f <- stf
        x <- stx
        return (f x)

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s ->
        let (x,s') = app st s in app (f x) s')  


-- Funciones para manipular el estado
get :: ST State
get = S (\s -> (s, s))

put :: State -> ST ()
put s = S (\_ -> ((), s))

-- Función de prueba
testST :: ST State
testST = do
    s <- get
    put (s + 1)
    get

-- Función fresh implementada con notación do
fresh :: ST State
fresh = do
    s <- get
    put (s + 1)
    return s 
