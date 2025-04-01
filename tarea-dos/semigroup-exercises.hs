-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778


import Data.Semigroup



--EJERCICIO 9  - COMBINE
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    --TO DO





--EJERCICIO 11 - VALIDATION I

data Validation a b = Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    --TO DO




--EJERCICIO 12 - VALIDATION II

newtype AccumulateRight a b = AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    --TO DO





--EJERCICIO 13 - VALIDATION III

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    --TO DO