-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module MonadsExercises where

{-
EJERCICIO 4 - Sección 12.5 pagina 175 y 176

4. There may be more than one way to make a parameterised type into an applicative functor. 
For example, the library Control. 
Applicative provides an alternative 'zippy' instance for lists, in which the function pure 
makes an infinite list of copies of its argument, and the operator <*> applies each argument 
function to the corresponding argument value at the same position. 

Complete the following declarations that implement this idea.

The ZipList wrapper around the list type is required because each type can only have at most 
one instance declaration for a given class.
-}

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    fmap _ (Z [])     = Z []
    fmap g (Z (x:xs)) = let Z ys = fmap g (Z xs)
                        in Z (g x : ys)

instance Applicative ZipList where 
    pure x = Z (let xs = x:xs in xs) 
    _ <*> Z [] = Z []
    Z [] <*> _ = Z []
    Z (f:fs) <*> Z (a:as) = Z (f a:(fs <*> as))

{-
EJERCICIO 8 - Sección 12.5 pagina 175 y 176

8. Rather than making a parameterised type into instances of the Functor, Applicative and Monad 
classes in this order, in practice it is sometimes simpler to define the functor and applicative 
instances in terms of the monad instance, relying on the fact that the order in which declarations 
are made is not important in Haskell. 

Complete the missing parts in the following declarations for the ST type using the do notation.
-}

type State = Int

newtype ST a = S (State -> (a,State))

app (S st) x = st x

instance Functor ST where
    fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do
        x <- st
        return (g x)

instance Applicative ST where
    pure x = S (\s -> (x,s))
    stf <*> stx = do
        f <- stf
        x <- stx
        return (f x)

instance Monad ST where
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

{-
EJERCICIO:
El siguiente ejercicio sobre la Monada ST a vista en el teorico, se pide implementar las 
siguientes funciones:
-}

get :: ST State
get = S (\s -> (s, s))

put :: State -> ST ()
put x = S (\_ -> ((), x))

testST :: ST State
testST = do
  s <- get
  put (s + 1)
  get

runTestST = app testST 3

{-
Fresh - Reimplementar utilizando la notacion do y las primitivas anteriores la funcion fresh vista en el teorico:
fresh :: ST Int
fresh = S (\n -> (n, n+1))
-}

fresh :: ST State
fresh = do
    n <- get
    put (n + 1)
    return n