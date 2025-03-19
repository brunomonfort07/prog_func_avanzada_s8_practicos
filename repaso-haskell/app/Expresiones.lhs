> module Expresiones where

> import Prelude hiding (lookup, emtpy)
> import Data.Either
> import Data.Map hiding (foldl)

Expresiones aritmeticas numericas

> data Operacion = Mult | Sum
>   deriving (Show, Enum, Eq, Ord)

Clase Num soporta suma y multiplicacion, high-order

> convert :: Num a => Operacion -> (a -> a -> a)
> convert Sum = (+)
> convert Mult = (*)

> data Expresion e r =
>    NumEntero e
>  | NumReal r
>  | Var String
>  | Op Operacion [Expresion e r]
>  deriving Show

Ejemplo de expresiones

> expEntera :: (Num e) => Expresion e r
> expEntera = NumEntero 10

> expReal :: (RealFloat r) => Expresion e r
> expReal = NumReal 20.9

> expSuma :: Expresion Int Float
> expSuma = Op Sum [expEntera, expReal, expReal]

> expSumaReal :: Expresion e Float
> expSumaReal = Op Sum [expReal, expReal]

Evaluador entero

> type Memoria n = Map String n

> evaluador :: (Integral a, RealFloat b) => Memoria a -> Expresion a b -> a
> evaluador m (NumEntero n) = n
> evaluador m (NumReal n) = round n
> evaluador m (Var x) = m ! x
> evaluador m (Op op exps) = foldl (\ acc exp -> convert op acc (evaluador m exp)) 0 exps

Ejemplos de evaluación

> -- >>> evaluador empty expSuma
> -- 52

Evaluador real

> evaluadorR :: (Integral a, RealFrac b) => Memoria b -> Expresion a b -> b
> evaluadorR m (NumEntero n) = fromIntegral n
> evaluadorR m (NumReal n) = n
> evaluadorR m (Var x) = m ! x
> evaluadorR m (Op op exps) = foldl (\ acc exp -> convert op acc (evaluadorR m exp)) 0 exps

> -- >>> evaluadorR empty expSuma
> -- 51.8

Queremos tener un sistema de tipos que no permita mezclar expresiones enteras con reales

> type Error = String

Creamos tipo para anotar una expresion como entera o real, la idea es que dentro de una expresion entera solo haya expresiones enteras e igual para expresiones reales

> data ExpresionTipada e r =
>   ExpresionEntera (Expresion e r)
>   | ExpresionReal (Expresion e r)
>   deriving Show

> esEntera :: ExpresionTipada e r -> Bool
> esEntera (ExpresionEntera _) = True
> esEntera _ = False

> data TipoVar = Entero | Real

> tipar :: (Integral e) => Memoria TipoVar -> Expresion e r -> Either Error (ExpresionTipada e r)
> tipar m e@(NumEntero n) = Right (ExpresionEntera e)
> tipar m e@(NumReal n) = Right (ExpresionReal e)
> tipar m e@(Var x)
>   | Just Entero <- lookup x m = Right (ExpresionEntera e)
>   | Just Real <- lookup x m = Right (ExpresionReal e)
>   | otherwise = Left ("Variable:" <> x <> " no declarada")
> tipar m e@(Op _ exps)
>   | all (either (const False) esEntera . tipar m) exps = Right (ExpresionEntera e)
>   | all (either (const False) (not . esEntera) . tipar m) exps = Right (ExpresionReal e)
>   | otherwise = Left "Expresiones enteras y reales mezcladas"

> -- >>> tipar empty expSuma
> -- >>> tipar empty expSumaReal
> -- Left "Expresiones enteras y reales mezcladas"
> -- Right (ExpresionReal (Op Sum [NumReal 20.9,NumReal 20.9]))



