{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module Laboratorio9 where

import Data.Foldable
import Prelude hiding (either)
import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified  Data.Map as M

{- 1. rDec is a function that should get its argument in the context of
Reader and return a value decremented by one. -}
rDec :: Num a => Reader a a
rDec = ReaderT (Identity . ((-) 1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (Identity . show)

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  ReaderT $ print . ("Hi: " ++) . show
  ReaderT (return . (+1))

rPrintAndInc' :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc' = do
  a <- ReaderT $ pure . id
  lift $ print $ "Hi: " ++ show a
  return $ a + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  a <- get
  lift $ print $ "Hi: " ++ show a
  put (a + 1)
  return $ show a
  
-- Fix the code
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

-- Monad transformers
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Functor f) => Functor (WriterT w f) where
  fmap :: Functor f => (a -> b) -> WriterT w f a -> WriterT w f b
  fmap f (WriterT fa) = WriterT $ (fmap . applyfst) f fa
    where applyfst f (a, b) = (f a , b)

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure = WriterT . pure . (, mempty)

  (<*>) :: (Monoid w, Monad m) => WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
  WriterT h <*> WriterT fa = WriterT $ do
    (f, w) <- h
    (a, w') <- fa
    return (f a , w <> w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  (>>=) :: (Monoid w, Monad m) => WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  WriterT m_aw >>= f = WriterT $ do
    (a, w) <- m_aw
    (b, w') <- runWriterT (f a)
    return (b, w <> w')

instance Monoid w => MonadTrans (WriterT w) where
  lift :: (Monoid w, Monad m) => m a -> WriterT w m a
  lift = WriterT . fmap (,mempty)

writeT :: Monad m => w -> WriterT w m ()
writeT w = WriterT $ pure ((), w)

data Expr = Var String -- variables
  | Num Int            -- literales numericos
  | Cte String         -- constantes  
  | Add Expr Expr
  | Div Expr Expr
  | Assign String Expr -- asignacion x := e
  deriving Show
  
evalLog :: Expr ->
           WriterT -- logea lo que va evaluando
            [Int]
            (StateT -- guardamos los valores de las variables
              (Map String Int) -- el estado es un mapeo entre nombres de variables y su valor entero asociado
              (ReaderT  -- vamos a recibir el valor de las constantes
                 (Map String Int)  -- la configuracion del valor de las constantes
                 (Either -- manejo de errores
                    String -- mensaje asociado a un error
                 )
            ))
            Int -- El valor que calculamos al evaluar la expresion
evalLog (Num n) = writeT [n] >> return n
evalLog (Add e e') = (+) <$> evalLog e <*> evalLog e'
evalLog (Div e e') = do
  n' <- evalLog e'
  if n' == 0
    then lift $ lift $ lift $ Left "division por cero"
    else do
      n <- evalLog e
      writeT [n `div` n']
      return (n `div` n')
evalLog (Var var) = do
  variables <- lift get
  case M.lookup var variables of
    Nothing -> lift $ lift $ lift $ Left "variable no inicializada"
    Just n -> writeT [n] >> return n
evalLog (Cte cte) = do
  constantes <- lift $ lift ask
  case M.lookup cte constantes of
    Nothing -> lift $ lift $ lift $ Left "constante no conocida"
    Just n -> writeT [n] >> return n
evalLog (Assign var e) = do
  n <- evalLog e
  variables <- lift get
  lift $ put $ M.insert var n variables
  writeT [n]  
  return n

ejemploExpr :: Expr
ejemploExpr = Assign "hola" (Num 2)
  
evalLogEjemploExpr :: Either String ((Int, [Int]), Map String Int)
evalLogEjemploExpr = runReaderT (runStateT (runWriterT (evalLog ejemploExpr)) (M.empty)) M.empty

