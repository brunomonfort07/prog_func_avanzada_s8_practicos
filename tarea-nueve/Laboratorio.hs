{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Laboratorio where

import Data.Foldable
import Prelude hiding (either)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified  Data.Map as M 

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  Identity a >>= f = f a

newtype IdentityT f a = IdentityT { runIdentityT :: f (Identity a) }

instance (Functor f) => Functor (IdentityT f) where
  fmap :: Functor f => (a -> b) -> IdentityT f a -> IdentityT f b
  fmap f (IdentityT fia) = IdentityT $ (fmap . fmap) f fia

instance (Applicative f) => Applicative (IdentityT f) where
  pure :: Applicative f => a -> IdentityT f a
  pure = IdentityT . pure . pure
  IdentityT h <*> IdentityT fa = IdentityT $ ((<*>) <$> h) <*> fa
  
instance (Monad m) => Monad (IdentityT m) where
  (>>=) :: Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  IdentityT mia >>= f = IdentityT $ mia >>= runIdentityT . f . runIdentity

instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a -- m (Identity a)
  lift = IdentityT . fmap Identity

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor f) => Functor (MaybeT f) where
  fmap f (MaybeT fa) = MaybeT $ (fmap . fmap) f fa

instance (Applicative f) => Applicative (MaybeT f) where
  pure = MaybeT . pure . pure
  MaybeT h <*> MaybeT fa = MaybeT $ ((<*>) <$> h) <*> fa

instance (Monad m) => Monad (MaybeT m) where
  (>>=) :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT m_my_a >>= f = MaybeT $ do
    my_a <- m_my_a
    case my_a of
      Nothing -> pure Nothing
      Just a -> runMaybeT (f a)
  
instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift m = MaybeT $ Just <$> m

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: MonadIO m => IO a -> MaybeT m a
  liftIO = MaybeT . liftIO . fmap Just
  
-- raise error
nothingT :: Applicative m => MaybeT m a
nothingT = MaybeT $ pure Nothing

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor f) => Functor (EitherT e f) where
  fmap f (EitherT fa) = EitherT $ (fmap . fmap) f fa

instance (Applicative f) => Applicative (EitherT e f) where
  pure = EitherT . pure . pure
  EitherT h <*> EitherT fa = EitherT $ ((<*>) <$> h) <*> fa

instance (Monad m) => Monad (EitherT e m) where
  (>>=) :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT m_e_a >>= f = EitherT $ do
    e_a <- m_e_a
    case e_a of
      Left e -> pure $ Left e
      Right a -> runEitherT $ f a

failT :: Applicative m => e -> EitherT e m a
failT e = EitherT $ pure $ Left e

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . (Right <$>)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor f) => Functor (StateT s f) where
  fmap :: Functor f => (a -> b) -> StateT s f a -> StateT s f b
  fmap g (StateT h) = StateT $ \ s -> fmap applyfst (h s)  -- :: f (a , s) -> f (g a , s)
    where applyfst (a, s) = (g a , s)

instance (Monad m) => Applicative (StateT s m) where
  pure :: Monad m => a -> StateT s m a
  pure a = StateT $ \ s -> pure (a, s)
  (<*>) :: Monad m => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT h <*> StateT g = StateT $ \ s -> do
    (f, s') <- h s
    (a, s'') <- g s'
    return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  (>>=) :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT h >>= g = StateT $ \ s -> do
    (a, s') <- h s
    runStateT (g a) s'

putT :: Monad m => s -> StateT s m ()
putT s = StateT (\ _ -> pure ((), s))

getT :: Monad m => StateT s m s
getT = StateT (\s -> pure (s,s))

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \ s -> do
    a <- m
    return (a, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: MonadIO m => IO a -> StateT s m a
  liftIO ioa = StateT $ \ s -> do
    a <- liftIO ioa
    return (a, s)

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor f => Functor (ReaderT r f) where
  fmap f (ReaderT g) = ReaderT (fmap f . g)

instance Applicative f => Applicative (ReaderT r f) where
  pure = ReaderT . pure . pure
  (<*>) :: Applicative f => ReaderT r f (a -> b) -> ReaderT r f a -> ReaderT r f b
  ReaderT f <*> ReaderT g = ReaderT ((<*>) <$> f <*> g)

instance Monad m => Monad (ReaderT r m) where
  (>>=) :: Monad m => ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  ReaderT f >>= g = ReaderT $ \ r -> do
     a <- f r
     runReaderT (g a) r

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . pure

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: MonadIO m => IO a -> ReaderT r m a
  liftIO = ReaderT . pure . liftIO

askT :: Applicative m => ReaderT r m r
askT = ReaderT pure

data Expr = Var String -- variables
  | Num Int            -- literales numericos
  | Cte String         -- constantes  
  | Add Expr Expr
  | Div Expr Expr
  | Assign String Expr -- asignacion x := e
  deriving Show

-- Ejemplo Stack de monadas State Reader Either
eval :: Expr ->
           StateT -- guardamos los valores de las variables
              (Map String Int) -- el estado es un mapeo entre nombres de variables y su valor entero asociado
              (ReaderT  -- vamos a recibir el valor de las constantes
                 (Map String Int)  -- la configuracion del valor de las constantes
                 (Either -- manejo de errores
                    String -- mensaje asociado a un error
                 )
              )
              Int -- El valor que calculamos al evaluar la expresion
        -- Either e (Reader map (State map Int))
              
eval (Num n) = return n
eval (Add e e') = (+) <$> eval e <*> eval e'
eval (Div e e') = do
  n' <- eval e'
  if n' == 0
    then lift $ lift $ Left "division por cero"
    else do
      n <- eval e
      return (n `div` n')
eval (Var var) = do
  variables <- getT
  case M.lookup var variables of
    Nothing -> lift $ lift $ Left "variable no inicializada"
    Just n -> return n
eval (Cte cte) = do
  constantes <- lift askT
  case M.lookup cte constantes of
    Nothing -> lift $ lift $ Left "constante no conocida"
    Just n -> return n
eval (Assign var e) = do
  n <- eval e
  variables <- getT
  putT $ M.insert var n variables
  return n

ejemploExpr :: Expr
ejemploExpr = Assign "hola" (Num 2)

evalEjemploExpr :: Either String (Int, Map String Int)
evalEjemploExpr = runReaderT (runStateT (eval ejemploExpr) (M.empty)) M.empty
  

