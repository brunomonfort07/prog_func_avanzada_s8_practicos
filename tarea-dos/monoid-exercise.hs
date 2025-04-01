-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

import Data.Monoid

newtype Mem s a =
    Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
    mempty = -- TO DO
    mappend = -- TO DO



f' = Mem $ \s -> ("hi", s + 1)
main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0