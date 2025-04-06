-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

{- 
Se piden
  Los ejercicios de semigrupos de las paginas 593 y 594, ejercicos 9 (Combine), 11, 12 y 13 (Validation).
  Luego el ejercicio de la pagina 596, ejercicio 8 (Mem)
-}

import Data.Monoid

-- Mem s a envuelve una función (runmem) que: 
--     Recibe un valor de tipo s (que podés pensar como el "estado actual"). Devuelve un par (a, s).
--     Devuelve un par (a, s):
--         a es un valor resultante.
--         s es el nuevo estado después de aplicar la función.
newtype Mem s a =
    Mem { runMem :: s -> (a, s) }

instance Monoid a => Semigroup (Mem s a) where
    (<>) (Mem f) (Mem g) = 
            Mem $ \s -> 
                let (a1, s1) = f s
                    (a2, s2) = g s1
                in (a1 <> a2, s2)

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)
main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0