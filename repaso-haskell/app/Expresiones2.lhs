> module Expresiones2 where

> import Data.Either

Expresiones aritmeticas numericas

> data Operacion = Sum | Mult
>   deriving Show

Clase Num soporta suma y multiplicacion

> convert :: Num a => Operacion -> (a -> a -> a)
> convert Sum = (+)
> convert Mult = (*)

> data Expresion a =
>   Numero a
>   | Op Operacion [Expresion a]
>  deriving Show

> instance Functor Expresion where
>   fmap f (Numero a) = Numero (f a)
>   fmap f (Op op exps) = Op op (map (fmap f) exps)

> -- >>> fmap (* 2) (Op Sum [Numero 2, Numero 3]) 
> -- Op Sum [Numero 4,Numero 6]

> instance Applicative Expresion where
>   pure = Numero
>   Numero f <*> t =  fmap f t
>   Op o exps <*> t = Op o (fmap (<*> t) exps)

> -- >>> (Op Sum [Numero (*2), Numero (*3)]) <*> (Op Mult [Numero 3])
> -- Op Sum [Op Mult [Numero 6],Op Mult [Numero 9]]

Ejemplo de expresion entera

> expEntera :: Expresion Int
> expEntera = Numero 1

> expReal :: Expresion Float
> expReal = Numero 23.9

Evaluador 

> evaluador :: (Num a) => Expresion a -> a
> evaluador (Numero n) = n
> evaluador (Op op exps) = foldl (\ acc exp -> convert op acc (evaluador exp)) 0 exps

Ejemplos de evaluación enteros

> -- >>> evaluador (Op Sum [Numero 2, Numero 3])
> -- 5


> -- >>> evaluador (Sum [Numero 2.0, Numero 2.9])




