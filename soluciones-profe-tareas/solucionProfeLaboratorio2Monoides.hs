import Data.Monoid

-- 9

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\x -> f x <> g x)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)

--11

data Validation a b = Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => 
    Semigroup (Validation a b) where
        Failure x <> Failure y = Failure (x <> y)
        Failure x <> _ = Failure x
        _ <> Failure y = Failure y
        Success x <> Success y = Success y

--12

newtype AccumulateRight a b = AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => 
    Semigroup (AccumulateRight a b) where
        AccumulateRight (Success x) <> AccumulateRight (Success y) = AccumulateRight (Success (x <> y))
        AccumulateRight (Success x) <> _ = AccumulateRight (Success x)
        _ <> AccumulateRight (Success y) = AccumulateRight (Success y)
        AccumulateRight (Failure x) <> AccumulateRight (Failure _) = AccumulateRight (Failure x)

--13

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
    Semigroup (AccumulateBoth a b) where
        AccumulateBoth (Success x) <> AccumulateBoth (Success y) = AccumulateBoth (Success (x <> y))
        AccumulateBoth (Failure x) <> AccumulateBoth (Success y) = AccumulateBoth (Failure x)
        AccumulateBoth (Success x) <> AccumulateBoth (Failure y) = AccumulateBoth (Failure y)
        AccumulateBoth (Failure x) <> AccumulateBoth (Failure y) = AccumulateBoth (Failure (x <> y))

--8 MEM

newtype Mem s a = Mem { runMem :: s -> (a,s)}

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)

instance Monoid a => Semigroup (Mem s a) where
    (<>) (Mem f) (Mem g) = Mem $ \s -> let (a1, s1) = f s
                                           (a2, s2) = g s1
                                       in (a1 <> a2, s2)



f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

g' :: Mem Int String
g' = Mem $ \s -> (" there", s + 2)

main :: IO ()
main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0

main2 :: IO ()
main2 = do
    print $ runMem (f' <> g') 0
