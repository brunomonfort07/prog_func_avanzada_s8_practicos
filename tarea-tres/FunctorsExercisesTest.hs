--NO SUBIR A LA TAREA - ES SOLO PARA PROBAR LA SOLUCION
--NO SUBIR A LA TAREA - ES SOLO PARA PROBAR LA SOLUCION
--NO SUBIR A LA TAREA - ES SOLO PARA PROBAR LA SOLUCION

import FunctorsExercises

-- Pruebas - Todas las leyes deben retornar True.
-- Ejecutar el comando "ghc FunctorsExercises.hs FunctorsExercisesTest.hs & runghc FunctorsExercisesTest.hs" para compilar ambas clases y luego ejecutar el "main".
main :: IO ()
main = do
    putStrLn "=== Ley de Identidad (fmap id x == x) ==="

    -- 1) Three
    let threeVal = Three "tag" False (42 :: Int)
    print $ fmap id threeVal == threeVal

    -- 2) Sum
    let sumVal = Second "hola" :: Sum Int String
    print $ fmap id sumVal == sumVal

    -- 3) LiftItOut
    let liftVal = LiftItOut (Just (7 :: Int))
        LiftItOut v1 = fmap id liftVal
        LiftItOut v2 = liftVal
    print $ v1 == (v2 :: Maybe Int)

    putStrLn "\n=== Ley de Composición (fmap (f . g) x == (fmap f . fmap g) x) ==="

    let f x = x * 2
        g x = x + 3

    -- 1) Three
    let threeVal2 = Three "tag" True (10 :: Int)
    print $ fmap (f . g) threeVal2 == (fmap f . fmap g) threeVal2

    -- 2) GoatLord
    let goatVal = MoreGoats (OneGoat (1 :: Int)) NoGoat (OneGoat 2)
    print $ fmap (f . g) goatVal == (fmap f . fmap g) goatVal

    -- 3) TalkToMe (caso Print)
    let pt = Print "x" (5 :: Int)
        Print s1 v1 = fmap (f . g) pt
        Print s2 v2 = (fmap f . fmap g) pt
    print $ s1 == s2 && v1 == v2