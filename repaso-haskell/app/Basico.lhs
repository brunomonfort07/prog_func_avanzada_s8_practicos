> module Basico where

> import qualified Data.Map as M

  
* Existing *types*

** Tuple

> tupla = (2, "hola")
> terna = (3, 4.5, [1,2])

** Lists

> lista = [2,3,4]

* Complrehension Lists, ranges

> -- >>> [1 .. 3]
> -- [1,2,3]

* Polimorfismo parametrico, Type Classes (Eq, Show, Num)

Clases ~ Conjuntos de tipos con cierta interfaz (Num, Eq, Ord, ...)

> suma :: (Num a, Ord a) => a -> a -> a
> suma x y = if x > y then x else x + y

> -- >>> suma 2 3
> -- 5

> -- >>> suma 2.0 3.4
> -- 5.4

* Data types

> data Lista a = Null | Cons a (Lista a)
>   deriving Show

> ejemploLista :: Lista Int 
> ejemploLista = Cons 2 (Cons 3 Null) -- [2,3]

> largo :: Lista a -> Int
> largo Null = 0
> largo (Cons _ xs) = 1 + largo xs

> largo2 :: [a] -> Int
> largo2 [] = 0
> largo2 (_ : xs) = 1 + largo2 xs

> sumaLista :: (Num a) => Lista a -> a
> sumaLista Null = 0
> sumaLista (Cons x xs) = x + sumaLista xs

> sumaLista2 :: (Num a) => Lista a -> a
> sumaLista2 x = case x of
>   Null -> 0
>   (Cons x xs) -> x + sumaLista xs

* Definicion de Funciones con guadras

> mayoresCeroLista :: (Num a, Ord a) => Lista a -> Lista a
> mayoresCeroLista Null = Null
> mayoresCeroLista (Cons x xs)
>   | x > constante = Cons (doble x) (mayoresCeroLista xs)
>   | otherwise = mayoresCeroLista xs
>   where
>     constante = 2
>     doble x = 2 * x

> -- >>> mayoresCeroLista (Cons 3 (Cons 1 Null))
> -- Cons 6 Null

> mayoresCeroLista2 :: (Num a, Ord a) => Lista a -> Lista a
> mayoresCeroLista2 Null = Null
> mayoresCeroLista2 (Cons x xs)
>   | x > constante =
>       let funcion x = 2 * x
>           pepe = 3
>       in Cons (funcion x) (mayoresCeroLista xs)
>   | otherwise = mayoresCeroLista xs
>   where constante = 2

> -- >>> mayoresCeroLista ejemploLista
> -- Cons 6 Null

> -- >>> largo (Cons 1 (Cons 0 Null))
> -- 2

> -- >>> largo2 [2,3]
> -- 2


* Guardass y pattern matching

* Curryied Functions and sections

> isUpperAlpha :: Char -> Bool
> isUpperAlpha x = x `elem` ['A'..'Z']

> isUpperAlphanum2 :: Char -> Bool
> isUpperAlphanum2 = (`elem` ['A'..'Z'])

Prefija una funcion f la aplico a los argumentos a y b

f a b

Cualquier funcion si la escribim entre `` la volvemos infija 

a `f` b

Puede definir funciones para ser usadas en forma infija sin las ``

(+) :: Int -> Int -> Int

a + b

Puedo tambien usar un operador que fue definido para ser usado de forma infija,  lo puedo usar de forma prefija

(+) a b

> -- (+) 2 3 

* Secciones de operadores infijos y lambda absractions

(+2) = \ x -> x + 2

(2+) = \ x -> 2 + x

* High order

> applyTwice :: (a -> a) -> a -> a
> applyTwice f  = f . f

> -- >>> applyTwice (+ 2) 1 
> -- 5

> -- >>> (+ 2) ((+ 2) 1)
> -- 5

* High Order fold map

> -- >>> map (+ 2) [1 .. 3]
> -- [3,4,5]


> -- >>> filter (\ x -> x > 10) [0 .. 20]  
> -- [11,12,13,14,15,16,17,18,19,20]


> -- >>> (\ x y -> x + y) 2 3
> -- 5

Fold left

  inicial ->  [ 1 , 2 , 3 ... ]
    f         (f inicial 1) --> res
                            (f res 2) --> res2
                                        (f res2 3) --> res3 

> suma3 :: (Num a) => [a] -> a
> suma3 = foldl (\ acc x -> acc + x) 0

                         ^           ^
                         |           |
                         f           inicial
                        

> -- >>> suma3 [1,2,3]
> -- 6

> suma4 :: (Num a) => [a] -> a
> suma4 = foldl (+) 0

> -- >>> suma4 [3,4]
> -- 7

> reverso :: [a] -> [a]
> reverso = foldl (flip (:)) []

> -- >>> reverso [1 .. 3]
> -- [3,2,1]

* data Maybe a = Nothing | Just a 

> abstraer :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
> abstraer Nothing _ = Nothing
> abstraer (Just m) f = f m

> problema2 :: Int -> M.Map Int Int -> Maybe Int
> problema2 n map = abstraer (M.lookup n map)  
>                   (\ m -> abstraer (M.lookup m map)
>                   (\ p -> M.lookup p map)
>                   )

> problema3 :: Int -> M.Map Int Int -> Maybe Int
> problema3 n map = M.lookup n map >>=
>                   (\ m -> M.lookup m map >>=
>                   (\ p -> M.lookup p map))

> -- >>> problema3 2 (M.fromList [(2,3), (3,4), (4,6)])        
> -- Just 6

> problema4 :: Int -> M.Map Int Int -> Maybe Int
> problema4 n map = do 
>   m <- M.lookup n map
>   p <- M.lookup m map
>   M.lookup p map

> -- >>> problemaMejorado 2 (M.fromList [(2,3), (3,4), (4,6)])        
> -- Just 6

> sumaMaybe :: Maybe Int -> Maybe Int -> Maybe Int
> sumaMaybe mx my = do  
>   x <- mx
>   y <- my
>   return (x + y)

> -- >>> sumaMaybe (Just 2) (Just 3)
> -- Just 5

data Either a b = Left a | Right b

> ejempoEither = Left 3 


> sumaEither2 :: Either String Int -> Either String Int -> Either String Int
> sumaEither2 ex ey = do
>   x <- ex
>   y <- ey
>   return (x + y)

> -- >>> sumaEither (Right 4) (Right 3) 
> -- Right 7

sumaMonada abstre e implementa ambas sumaEither y sumaMaybe

> sumaMonada :: (Integral a, Monad m) => m a -> m a -> m a
> sumaMonada mx my = do
>   x <- mx
>   y <- my
>   return (x + y)

> divError :: Integral a => a -> a -> Either String a
> divError m 0 = Left "quisiste divir por cero"
> divError m n = Right (div m n)

> -- >>> sumaMonada (Right 3) (divError 3 0)
> -- Left "quisiste divir por cero"

> -- >>> divError 4 (-4)
> -- Right (-1)

> sumaMonadaLista :: (Integral a, Monad m) => [m a] -> m a
> sumaMonadaLista []       = return 0
> sumaMonadaLista (mx : xs) = do
>   x <- mx
>   y <- sumaMonadaLista xs
>   return (x + y)

> -- >>> sumaMonadaLista [Right 3, Right 2, divError 3 0]

> sumaMonadaLista2 :: (Integral a, Monad m) => [m a] -> m a
> sumaMonadaLista2 = foldl fun (return 0)
>   where
>     fun macc mx = do
>       acc <- macc
>       x <- mx
>       return (acc + x)

> -- >>> sumaMonadaLista2 [Right 3, Right 2]
> -- Left "quisiste divir por cero"

