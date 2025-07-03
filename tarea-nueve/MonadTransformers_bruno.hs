-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778

module MonadTransformers where

import Laboratorio
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Data.Map (Map)
import qualified  Data.Map as M

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure b = Reader (\ _ -> b)
  (Reader f) <*> (Reader g) = Reader (\ a -> f a (g a))

instance Monad (Reader r) where
  (>>=) :: Reader a b -> (b -> Reader a c) -> Reader a c
  Reader f >>= g = Reader (\ a -> runReader (g (f a)) a)

--Capítulo 26 del Allen, sección 26.14, ejercicos: "Write the code"  y "Fix the code", páginas 964-965.
{- 
WRITE THE CODE
1. 
rDec is a function that should get its argument in the context of
Reader and return a value decremented by one.
rDec :: Num a => Reader a a
rDec = undefined
Prelude> import Control.Monad.Trans.Reader
Prelude> runReader rDec 1
0
Prelude> fmap (runReader rDec) [1..10] == (runReader rDec) <$> [1..10]
[0,1,2,3,4,5,6,7,8,9]
Note that “Reader” from transformers is actually ReaderT of
Identity and that runReader is a convenience function throwing
away the meaningless structure for you. Play with runReaderT
if it tickles your nondescript furry red puppet.
-}

rDec :: Num a => Reader a a
rDec = Reader (\n -> n-1)

{- 2. Once you have an rDec that works, make it and any inner lambdas
pointfree if that’s not already the case.
"Hay que sacar el \n"
-}

rDecPointFree :: Num a => Reader a a
rDecPointFree = Reader (subtract 1)
-- Tambien funciona con (+(-1))

{- 3. 
rShow is show, but in Reader.
rShow :: Show a => ReaderT a Identity String
rShow = undefined
Prelude> runReader rShow 1
"1"
Prelude> fmap (runReader rShow) [1..10]
["1","2","3","4","5","6","7","8","9","10"]
-}


--Reader Int String ≡ ReaderT Int Identity String
{-
Es decir, runReader es un atajo para:
    - Ejecutar el ReaderT con runReaderT, que te da un Identity a,
    - Sacar el valor del Identity con runIdentity.
-}
rShow :: Show a => ReaderT a Identity String -- = Reader Int String
rShow = ReaderT (\a -> Identity $ show a) 

{- 4. Once you have an rShow that works, make it pointfree.
-}
rShowPointFree :: Show a => ReaderT a Identity String -- = Reader Int String
rShowPointFree = ReaderT (Identity . show) 

{- 5. 
rPrintAndInc will first print the input with a greeting, then return the input incremented by one.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = undefined
Prelude> runReaderT rPrintAndInc 1
Hi: 1
2
Prelude> traverse (runReaderT rPrintAndInc) [1..10]
Hi: 1
Hi: 2
Hi: 3
Hi: 4
Hi: 5
Hi: 6
Hi: 7
Hi: 8
Hi: 9
Hi: 10
[2,3,4,5,6,7,8,9,10,11]
-}

--runReaderT :: r -> m a
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT (\a -> do
    putStrLn $ "Hi: " ++ show a
    return $ a + 1)

{- 6. 
sPrintIncAccum first prints the input with a greeting, then puts
the incremented input as the new state, and returns the original
input as a String.
Prelude> runStateT sPrintIncAccum 10
Hi: 10
("10",11)
Prelude> mapM (runStateT sPrintIncAccum) [1..5]
Hi: 1
Hi: 2
Hi: 3
Hi: 4
Hi: 5
[("1",2),("2",3),("3",4),("4",5),("5",6)]
-}
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT (\a -> do
    putStrLn $ "Hi: " ++ show a
    return (show a, a + 1))

{- 
FIX THE CODE
The code won’t typecheck as written; fix it so that it does. Feel free to
 add imports if it provides something useful. Functions will be used
 that we haven’t introduced. You’re not allowed to change the types asserted. 
 
 You may have to fix the code in more than one place.
-}

isValid :: String-> Bool
isValid v = '!' `elem` v

-- guard produce la excepción cuando la condición es falsa (guard llama a mzero, que 
-- en IO falla). No hay una instancia de Alternative apropiada para manejar eso graciosamente.
maybeExcite :: Control.Monad.Trans.Maybe.MaybeT IO String
maybeExcite = Control.Monad.Trans.Maybe.MaybeT $ do
    v <- getLine
    if isValid v
        then return $ Just v
        else return Nothing

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- Control.Monad.Trans.Maybe.runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e -> putStrLn ("Good, was very excite: " ++ e)

{-
Se pide tambien escribir el Monad Transfromer de Writer, y sus instancias de Functor, 
Applicative, Monad y MonadTrans. Para la instancia de Applicative usar la instancia de 
monada, y no la de applicative como usualmente se hace,  al igual que se hizo en clase 
cuando se definió la instancia Applicative para StateT.
-}

--https://hackage-content.haskell.org/package/transformers-0.6.2.0/docs/Control-Monad-Trans-Writer-Lazy.html#g:2

--Libro: Cap 936
newtype Writer w a = Writer { runWriter :: (a, w) }

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Functor f) => Functor (WriterT s f) where
  fmap :: Functor f => (a -> b) -> WriterT s f a -> WriterT s f b
  fmap g (WriterT h) = WriterT $ fmap (applyfst g) h
    where applyfst g (a, s) = (g a, s)

instance (Monad m, Monoid s) => Applicative (WriterT s m) where
  --pure = Queremos tomar un valor a y devolver un WriterT w m a, con log vacío.
  pure :: Monad m => a -> WriterT s m a
  pure a = WriterT $ pure (a, mempty)
  -- apply = Queremos aplicar una función que viene en el contexto WriterT a un valor 
  -- también en ese contexto, acumulando los logs.
  (<*>) :: Monad m => WriterT s m (a -> b) -> WriterT s m a -> WriterT s m b
  WriterT h <*> WriterT g = WriterT $ do
    (f, s') <- h
    (a, s'') <- g
    return (f a, s' <> s'')

instance (Monad m, Monoid s) => Monad (WriterT s m) where
  --Operador de secuencia o bind
  --encadenar operaciones, acumulando el log a medida que se avanza
  (>>=) :: Monad m => WriterT s m a -> (a -> WriterT s m b) -> WriterT s m b
  WriterT h >>= g = WriterT $ do
    (a, s') <- h
    (b, s'') <- runWriterT $ g a
    return (b, s' <> s'')

--WRITER OPERATIONS
--tell w is an action that produces the output w.
tell :: Monad m => w -> WriterT w m ()
tell w = WriterT $ do 
    return ((), w)

{-
Usar el monad transformer definido anteriormente para extender el ejemplo de expresiones 
visto en el teórico, agregando al stack de monadas la monada definida anteriormente,  
de forma de agregar el efecto de tracing o logging al ejemplo visto en clase. Se pide 
también modificar el evaluador para que logee cada vez que se calcula un resultado 
intermedio.
-}

instance Monoid s => MonadTrans (WriterT s) where
  lift :: Monad m => m a -> WriterT s m a
  lift ma = WriterT $ do
    a <- ma
    return (a, mempty)

-- Ejemplo Stack de monadas Writer State Reader Either

eval3000 :: Expr ->
                WriterT [String]
                    (StateT -- guardamos los valores de las variables
                       (Map String Int) -- el estado es un mapeo entre nombres de variables y su valor entero asociado
                       (ReaderT  -- vamos a recibir el valor de las constantes
                          (Map String Int)  -- la configuracion del valor de las constantes
                          (Either -- manejo de errores
                             String -- mensaje asociado a un error
                          )
                       )
                    )
                    Int -- El valor que calculamos al evaluar la expresion
            -- Either e (Reader map (State map Int))

eval3000 (Num n) = return n
eval3000 (Add e e') = do
    n <- eval3000 e
    m <- eval3000 e'
    tell ["Found this dude trying to Add" ++ " = " ++ show (n + m)]
    return (n + m)
eval3000 (Div e e') = do
  n' <- eval3000 e'
  if n' == 0
    then lift $ lift $ lift $ Left "division por cero"
    else do
      n <- eval3000 e
      tell ["Found this dude trying to Div" ++ " = " ++ show (n `div` n')]
      return (n `div` n')
eval3000 (Var var) = do
  variables <- lift Laboratorio.getT
  case M.lookup var variables of
    Nothing -> lift $ lift $ lift $ Left "variable no inicializada"
    Just n -> return n
eval3000 (Cte cte) = do
  constantes <- lift $ lift askT
  case M.lookup cte constantes of
    Nothing -> lift $ lift $ lift $ Left "constante no conocida"
    Just n -> return n
eval3000 (Assign var e) = do
  n <- eval3000 e
  variables <- lift getT
  lift $ putT $ M.insert var n variables
  return n

ejemploExpr3000 :: Expr
ejemploExpr3000 = Assign "hola" (Num 2)

eval3000EjemploExpr :: Either String ((Int, [String]), Map String Int)
--eval3000EjemploExpr = runReaderT (runStateT (eval3000 ejemploExpr3000) (M.empty)) M.empty
eval3000EjemploExpr = runReaderT (runStateT (runWriterT (eval3000 ejemploExpr3000)) M.empty) M.empty

ejAdd = Add (Num 2) (Num 3)
testAdd = runReaderT (runStateT (runWriterT (eval3000 ejAdd)) M.empty) M.empty
--Ejecutando "testAdd" en la consola debería aparecer "Found this dude trying to Add = 5"