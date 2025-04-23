--NO SUBIR A LA TAREA - ES SOLO PARA PROBAR LA SOLUCION
--NO SUBIR A LA TAREA - ES SOLO PARA PROBAR LA SOLUCION
--NO SUBIR A LA TAREA - ES SOLO PARA PROBAR LA SOLUCION

import ApplicativesExercises
import Data.Semigroup

-- Valores de prueba
v' :: Three [String] (Sum Int) Int
v' = Three ["log"] (Sum 1) 10

u' :: Three [String] (Sum Int) (Int -> Int)
u' = Three ["u"] (Sum 2) (+2)

w' :: Three [String] (Sum Int) Int
w' = Three ["w"] (Sum 3) 5

-- Pruebas - Todas las leyes deben retornar True.
-- Ejecutar el comando "ghc ApplicativesExercises.hs ApplicativesExercisesTest.hs & runghc ApplicativesExercisesTest.hs" para compilar ambas clases y luego ejecutar el "main".
main :: IO ()
main = do
    putStrLn "PRUEBAS VALIDATION APPLICATIVE:"

    let v = Success 10 :: Validation [String] Int
    let u = Success (*2) :: Validation [String] (Int -> Int)
    let w = Success 3 :: Validation [String] Int
    let err1 = Failure ["Nombre requerido"]
    let err2 = Failure ["Edad inválida"]

    putStrLn "=== Ley de Identidad ==="
    print $ (pure id <*> v) == v

    putStrLn "=== Ley de Composición ==="
    print $ (pure (.) <*> u <*> pure (+1) <*> w) == (u <*> (pure (+1) <*> w))

    putStrLn "=== Ley de Homomorfismo ==="
    print $ (pure (+3) <*> pure 7 :: Validation [String] Int) == pure ((+3) 7)

    putStrLn "=== Ley de Intercambio ==="
    print $ (u <*> pure 4) == (pure ($ 4) <*> u)

    putStrLn "=== Combinación de errores ==="
    print $ (err1 <*> err2) == (Failure ["Nombre requerido", "Edad inválida"] :: Validation [String] Int)
    
    putStrLn ""
    putStrLn "PRUEBAS THREE APPLICATIVE:"
    -- Identity
    putStrLn "=== Ley de Identidad ==="
    print $ show ((pure id <*> v') == v')

    -- Composition
    let u'' = Three ["u"] (Sum 1) (*2)
        v'' = Three ["v"] (Sum 2) (+3)
        w'' = Three ["w"] (Sum 3) 4
        lhs = pure (.) <*> u'' <*> v'' <*> w''
        rhs = u'' <*> (v'' <*> w'')

    putStrLn "=== Ley de Composición ==="
    print $ show (lhs == rhs)

    -- Homomorphism
    let f = (*2)
        x = 5 :: Int
    putStrLn "=== Ley de Homomorfismo ==="
    print $ show ((pure f <*> pure x) == (pure (f x) :: Three [String] (Sum Int) Int))
    
    putStrLn "=== Ley de Intercambio ==="
    let y = Three ["log"] (Sum 3) (+100)
    print $ (y <*> pure 1) == (pure ($ 1) <*> y)

    -- Ver que mempty aparece en pure
    print $ (pure "hola" :: Three [String] (Sum Int) String) == Three [] (Sum 0) "hola"

    putStrLn "=== Ley de Composición ==="
    -- Ver composición de logs y sumas
    let f = Three ["f"] (Sum 1) (*2)
        x = Three ["x"] (Sum 10) 3
    print $ (f <*> x) == Three ["f", "x"] (Sum 11) 6