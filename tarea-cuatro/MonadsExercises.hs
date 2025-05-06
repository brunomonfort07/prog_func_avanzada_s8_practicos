-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module MonadsExercises where

--Libro: Programming in Haskell, 2nd Edition

{-
Se pide resolver los siguientes ejercicios de la seccion 12.5 pagina 175 y 176

ejercicio 4
ejercicio 8
Y el siguiente ejercicio sobre la Monada ST a vista en el teorico, se pide implementar las siguientes funciones:

get :: ST State
retorna el actual estado
put :: State -> ST ()
pisa el estado con el parametro dado retornando unit
Testear sus funciones get y put con la siguiente prueba
testST :: ST State
testST = do
  -- obtiene el estado en s
  s <- get
  -- suma uno y sobrescribe el estado
  put (s + 1)
  -- devuelve el estado actual
  get

runTestST = app testST 3
runTestST deberia devolver la tupla (4,4)

fresh :: ST State
reimplementar utilizando la notacion do y las primitivas anteriores la funcion fresh vista en el teorico.
-}

--Definicion Monad
--class Applicative m => Monad m where
    --return :: a -> ma
    --(>>=) :: ma -> (a -> mb) -> mb
    --return = pure



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
    -- Aplicar fmap sobre Z xs debe significar: aplicar g a cada elemento de xs, y 
    -- devolver un nuevo ZipList con los resultados.
    fmap _ (Z [])     = Z []
    fmap g (Z (x:xs)) = let Z ys = fmap g (Z xs)
                        in Z (g x : ys)

instance Applicative ZipList where
    pure x = Z (let xs = x:xs in xs) -- la lista empieza con x, y el resto de la lista es la misma lista (xs). Es una forma perezosa de decir "esto se repite para siempre".
    _ <*> Z [] = Z []
    Z [] <*> _ = Z []
    Z (f:fs) <*> Z (a:as) = Z (f a:(fs <*> as))
    