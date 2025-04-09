-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

{- 
Se piden
  Los ejercicios de semigrupos de las paginas 593 y 594, ejercicos 9 (Combine), 11, 12 y 13 (Validation).
  Luego el ejercicio de la pagina 596, ejercicio 8 (Mem)
-}

import Data.Monoid

newtype Mem s a =
    Mem { runMem :: s -> (a, s) }


-- Originalemnte lo hice de esta forma, pero me saltaba error
-- porque aparentemente en versiones nuevas de Hasckell no lo permite
--instance Monoid a => Monoid (Mem s a) where
--    mempty = Mem $ \s -> (mempty, s) 
--    mappend (Mem f) (Mem g) = Mem $ \s ->
--        let (a1, s1) = f s 
--            (a2, s2) = g s1 
--        in (a1 <> a2, s2)

instance Semigroup a => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = Mem $ \s ->
        let (a1, s1) = f s
            (a2, s2) = g s1
        in (a1 <> a2, s2)

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s) 


f' = Mem $ \s -> ("hi", s + 1)
main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0