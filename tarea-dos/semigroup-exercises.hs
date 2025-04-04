-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

{- 
Se piden
  Los ejercicios de semigrupos de las paginas 593 y 594, ejercicos 9 (Combine), 11, 12 y 13 (Validation).
  Luego el ejercicio de la pagina 596, ejercicio 8 (Mem)
-}

import Data.Semigroup
import Language.Haskell.TH (isInstance)

--EJERCICIO 9  - COMBINE
-- Dado un tipo de dato, implemente la instancia Semigroup. 
newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

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
    (Failure x) <> (Failure y) = Failure (x <> y)
    (Failure x) <> (Success y) = Success y
    (Success x) <> (Failure y) = Success x
    (Success x) <> (Success y) = Success y

        
--EJERCICIO 12 - VALIDATION II

newtype AccumulateRight a b = AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    (AccumulateRight (Failure x)) <> (AccumulateRight (Failure y)) = AccumulateRight (Failure (x ++ y))
        





--EJERCICIO 13 - VALIDATION III

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    --TO DO
