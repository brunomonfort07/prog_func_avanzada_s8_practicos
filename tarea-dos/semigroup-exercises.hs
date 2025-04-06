-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

{- 
Se piden
  Los ejercicios de semigrupos de las paginas 593 y 594, 
      - ejercicos 9 (Combine), 11, 12 y 13 (Validation).
  Luego el ejercicio de la pagina 596, ejercicio 8 (Mem)
-}

import Data.Semigroup

--EJERCICIO 9  - COMBINE
-- Dado un tipo de dato, implemente la instancia Semigroup. 
newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (<>) (Combine f) (Combine g) = Combine (\x -> f x <> g x)
    -- Lo anterior es la forma optimizada de esta: (<>) = \cf cg -> Combine $ \x -> (unCombine cf) x <> (unCombine cg) x


f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

{-
Prueba. Copiar y pegar en GHCi:
    Prelude> unCombine (f <> g) $ 0
Debería dar:
    Sum {getSum = 0} OK

Prueba. Copiar y pegar en GHCi:
    Prelude> unCombine (f <> g) $ 1
Debería dar:
    Sum {getSum = 2} OK

Prueba. Copiar y pegar en GHCi:
    Prelude> unCombine (f <> f) $ 1
Debería dar:
    Sum {getSum = 4} OK

Prueba. Copiar y pegar en GHCi:
    Prelude> unCombine (g <> f) $ 1
Debería dar:
    Sum {getSum = 2} OK
-}

--EJERCICIO 11 - VALIDATION I

data Validation a b = Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (<>) x y = case (x, y) of
      (Success s, Success _) -> Success s
      (Success s, Failure _) -> Success s
      (Failure _, Success s) -> Success s
      (Failure fx, Failure fy) -> Failure (fx <> fy)
-- Semigroup still provides a binary associative operation, one that typ
-- ically joins two things together (as in concatenation or summation),
-- but doesn’t have an identity value.


--EJERCICIO 12 - VALIDATION II - wrapper AccumulateRight

--AccumulateSuccess
{-Cuando hay un Success, se intenta preservar o acumularlo. 
  Pero si hay un Failure, se deja de intentar y se devuelve inmediatamente. 
  No se acumulan los errores (como sí lo haría AccumulateLeft).-}
newtype AccumulateRight a b = AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    (<>) x y = case (x, y) of
        (AccumulateRight (Success s1), AccumulateRight (Success s2)) 
            -> AccumulateRight (Success (s1 <> s2))
        (AccumulateRight (Success _), AccumulateRight (Failure f))
            -> AccumulateRight (Failure f)
        (AccumulateRight (Failure f), _)
            -> AccumulateRight (Failure f)

--EJERCICIO 13 - VALIDATION III

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    (<>) x y = case (x, y) of
        (AccumulateBoth (Success s1), AccumulateBoth (Success s2)) 
            -> AccumulateBoth (Success (s1 <> s2))
        (AccumulateBoth (Success _), AccumulateBoth (Failure f))
            -> AccumulateBoth (Failure f)
        (AccumulateBoth (Failure f), AccumulateBoth (Success _))
            -> AccumulateBoth (Failure f)
        (AccumulateBoth (Failure f1), AccumulateBoth (Failure f2)) 
            -> AccumulateBoth (Failure (f1 <> f2))