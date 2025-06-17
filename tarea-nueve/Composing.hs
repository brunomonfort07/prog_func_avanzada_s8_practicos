{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Composing where

import Data.Foldable
import Prelude hiding (either)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

{-
* Composing Types:

- We can compose functors and applicatives and get another functor and applicative,
this do not happen with Monads.

- However, there are many times in “real code” when composing mon-
ads is desirable. Different monads allow us to work with different
effects. Composing monads allows you to build up computations with
multiple effects. By stacking, for example, a Maybe monad with an
IO, you can be performing IO actions while also building up com-
putations that have a possibility of failure, handled by the Maybe
monad.

- A monad transformer is a variant of an ordinary type that takes an
additional type argument which is assumed to have a monad instance.
For example, MaybeT is the transformer variant of the Maybe type.
The transformer variant of a type gives us a Monad instance that binds
over both bits of structure.

-}


-- Identity

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)


-- ejemplo
ejemploIdentity :: Identity Int -- = Indentity Int = Int
ejemploIdentity = Identity 3 

{-

>:t Identity
Identity :: a -> Identity a

> :k Identity
Identity :: * -> *

identity :: a -> a
identity a = a
-}


-- Composing types: f . g = Compose at type level
--Compose :: (* -> *) -> (* -> *) -> * -> *
--              |           |        |
--              | +---------+        |
--              | | +----------------+
--              V V V
newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

{-

> :k Compose
Compose :: (* -> *) -> (* -> *) -> * -> *

ghci> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

-}
--                          f       g      a
composeExample' :: Compose Identity Maybe Int -- Identity (Maybe Int) = Maybe Int
composeExample' = Compose (Identity (Just 10))


-- Examples               f   g    a       f (g a)         
composeExample :: Compose [] Maybe Int -- [Maybe Int]
composeExample = Compose [Nothing, Just 1, Just 10]


{-

f ~ []
g ~ Maybe
a ~ Int

We have one bit of structure wrapped around another, then a value
type (the 𝑎) because the whole thing still has to be kind * in the end.
We’ve made the point in previous chapters that type constructors
are functions. Type constructors can take other type constructors as
arguments, too, just as functions can take other functions as arguments.
This is what allows us to compose types.
-}

{-
LIFTING
Let’s start with composing functors, using the types we’ve seen just
above. We know we can lift over Identity; you’ve seen this Functor
before:

-}

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
  
--instance (Functor f, Functor g) => Functor (Compose f g) where
--  fmap f (Compose fga) = Compose $ fmap (fmap f) fga
--    ::            ::               ::
--  (a -> b)     f (g a)          f (g b)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose ((fmap . fmap) f fga)

-- Example using previous functor
functorComposeExample = (+1) <$> composeExample

{- Lifting applicatives: Composing Applicatives -}
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: (Applicative f, Applicative g) => a -> Compose f g a -- f (g a)
  pure = Compose . pure . pure
  (<*>) :: (Applicative f, Applicative g) => Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose h) <*> (Compose fga) = Compose $ ((<*>) <$> h) <*> fga
  --      ::               ::                      ::
  --   f (g (a -> b))    f (g a)                 f (g b)
 
  -- Para usar el applicative de f, necesitamos  f (g a -> g b)
  --                  |
  --                  V
  -- <*> :: f (g a -> g b) -> f (g a) -> f (g b)
  --         ?           

  -- Como obtengo f (g a -> g b) ? a partir de f (g (a -> b)) . Notar: tenemos que usar el applicative de g aun

  -- Applicative de g tiene tipo  <*> :: g (a -> b) -> g a -> g b = g (a -> b) -> (g a -> g b)

  -- usando el functor de f le hacemos lift al applicative anterior en g

  --  f (g (a -> b)) -> f (g a -> g b)
  --        ::
  --        f                      
  -- Listo entonces hacemos fmap de h con el applicative de g tenemos ((<*>) <$> h) :: f (g a -> g b)  que es lo que necesitabamos

-- Example using previous functor
applicativeComposeExample = Compose [Just (+1), Nothing, Just (*2)] <*> composeExample


-- Exercises: Composable Foldable, Traversable

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  -- fold :: Monoid a => Compose f g a -> a
  fold = foldMap fold . getCompose
  -- foldMap :: Monoid b => (a -> b) -> Compose f g a -> b
  foldMap f = foldMap (foldMap f) . getCompose
  -- foldr :: (a -> b -> b) -> b -> Compose f g a -> b
  -- foldr :: (a -> b -> b) -> b -> g a -> b
  -- foldl :: (b -> g a -> b) -> b -> f (g a) -> b
  foldr f b = foldl (foldr f) b . getCompose
  -- foldl :: (b -> a -> b) -> b -> Compose f g a -> b
  -- foldl :: (b -> a -> b) -> b -> g a -> b
  -- foldl :: (b -> g a -> b) -> b -> f (g a) -> 
  foldl f b = foldl (foldl f) b . getCompose


instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  --traverse :: (Traversable f, Traversable g, Applicative f1) => (a -> f1 b) -> Compose f g a -> f1 (Compose f g b)
  traverse h (Compose fga) = Compose <$> traverse (traverse h) fga




{-
Monad transformers

The problem with Monad is: you can put two
together but you can’t get a new Monad instance out of it. When we
need to get a new Monad instance, we need a monad transformer. It’s
not magic; the answer is in the types.

We said above that a monad transformer is a type constructor that
takes a Monad as an argument and returns a Monad as a result. We also
noted that the fundamental problem with composing two Monads
lies in the impossibility of joining two unknown Monads.

In order to make that join happen, we need to reduce the polymorphism
and get concrete information about one of the Monads that we’re
working with. The other Monad remains polymorphic as a variable
type argument to our type constructor. Transformers help you make
a monad out of multiple (2, 3, 4...) types that each have a Monad
instance by wrapping around existing monads that provide each bit
of wanted functionality.

-}

-- newtype IdentityT f a = IdentityT { runIdentityT :: f a }

-- instance (Functor f) => Functor (IdentityT f) where
--   fmap f (IdentityT fa) = IdentityT $ fmap f fa

-- instance (Applicative f) => Applicative (IdentityT f) where
--   pure = IdentityT . pure 
--   IdentityT h <*> IdentityT fa = IdentityT $ h <*> fa
  
-- instance (Monad m) => Monad (IdentityT m) where
--   (>>=) :: Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
--   IdentityT ma >>= f = IdentityT $ ma >>= (runIdentityT . f)

-- IdentitT
-- The identity monad transformer, serving only to
-- to specify that additional structure should exist.  f   g
newtype IdentityT' f a = IdentityT' { runIdentityT' :: f (Identity a) }

instance (Functor f) => Functor (IdentityT' f) where
  fmap :: Functor f => (a -> b) -> IdentityT' f a -> IdentityT' f b
  fmap f (IdentityT' fia) = IdentityT' $ (fmap . fmap) f fia

instance (Applicative f) => Applicative (IdentityT' f) where
  pure :: Applicative f => a -> IdentityT' f a
  pure = IdentityT' . pure . pure
  IdentityT' h <*> IdentityT' fa = IdentityT' $ ((<*>) <$> h) <*> fa
  
instance (Monad m) => Monad (IdentityT' m) where
  (>>=) :: Monad m => IdentityT' m a -> (a -> IdentityT' m b) -> IdentityT' m b
  IdentityT' mia >>= f = IdentityT' $ mia >>= runIdentityT' . f . runIdentity

instance MonadTrans IdentityT' where
  lift :: Monad m => m a -> IdentityT' m a -- m (Identity a)
  lift = IdentityT' . fmap Identity


exampleIdentityT :: IdentityT' Maybe Integer -- Maybe (Identity Integer)
exampleIdentityT = IdentityT' (Just (Identity 1))

exampleIdentityTBetter :: IdentityT' Maybe Integer
exampleIdentityTBetter = pure 1 -- = IdentityT' ( pure ( pure 1)) = IdentityT' ( Just (Identity 1))
--                                                Maybe Identity

exampleUseIdentityFunctorT' :: IdentityT' Maybe Integer
exampleUseIdentityFunctorT' = (+1) <$> pure 1 -- ==  IdentityT' (Just (Identity 2))

exampleUseIdentityMonadT' :: IdentityT' Maybe Integer
exampleUseIdentityMonadT' = do -- ==  IdentityT' (Just (Identity 2))
  a <- pure 1
  lift Nothing -- = IdentityT' Nothing
  return (a + 1)

exampleUseIdentityMonadT2 :: IdentityT' [] Integer -- [Identity Integer]
exampleUseIdentityMonadT2 = do
  a <- lift [1, 2 , 3]
  lift []
  return (a + 1)
  -- = [Identity 2 , Identity 3, Identity 4]

-- Recovering Idenitity from IdentityT
type Identity' a = IdentityT' Identity a

exampleIdentity' :: Identity' Int
exampleIdentity' = IdentityT' (Identity (Identity 1))

{-

The essential extra of Monad transformers

IdentityT monad transformer actually captures the essence of transformers generally.

-}

-- MaybeT
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
      
-- m = []
composeExampleMaybeT :: MaybeT [] Int
composeExampleMaybeT = MaybeT [Nothing, Just 1, Just 10]

functorExampleMaybeT = (+1) <$> composeExampleMaybeT

applicativeExampleMaybeT = MaybeT [Just (+1), Nothing , Just (*2)] <*> composeExampleMaybeT

monadExampleMaybeT :: MaybeT [] Int
monadExampleMaybeT = do
  a <- composeExampleMaybeT
  --nothingT
  lift []
  return (a + 1)

-- diferencia ? fue por el nothing de composeExampleMaybeT hace corto cicuito y no ejecuta el lift []

ej :: [Maybe Int]
ej = do
  a <- [Nothing, Just 1, Just 10]
  b <- []
  return (fmap (+1) a)

applicativeExampleMaybeTMonadic :: MaybeT [] Int  
applicativeExampleMaybeTMonadic = do
  a <- composeExampleMaybeT
  f <- MaybeT [Just (+1), Nothing, Just (*2)]
  return (f a)
  
-- Recovering Maybe from MaybeT
type Maybe' a = MaybeT Identity a

exampleMaybe' :: Maybe' Int
exampleMaybe' = MaybeT (Identity (Just 1))

-- Monada state (repaso)
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap g st = State (\s -> let (x,s') = runState st s in (g x, s'))

instance Applicative (State s) where
  pure x = State (\s -> (x,s))
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = State
                  (\s ->
                     let (f,s') = runState stf s
                         (x,s'') = runState stx s'
                     in (f x, s'')
                  )

instance Monad (State s)  where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = State (\s ->
                      let (x,s') = runState st s
                      in runState (f x) s'
                   )

put :: s -> State s ()
put s = State (\ _ -> ((), s))

get :: State s s
get = State (\s -> (s,s))


-- Ejemplo Monada de estado con error
monadExampleMaybeTState :: MaybeT (State Int) Int
monadExampleMaybeTState = do
  lift $ put 1
  a <- lift get
  --nothingT --  some error
  lift $ put 2
  return a

-- runState (runMaybeT monadExampleMaybeTState) 3
    
-- EitherT
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

ejemploEitherT :: EitherT String Maybe Int -- Maybe (Either String Int)
ejemploEitherT = pure 2 
-- ejemploEitherT = EitherT (Just (Right 2))

ejemploMapEitherT = (+1) <$> ejemploEitherT

ejemploMonaEitherT :: EitherT String Maybe Int
ejemploMonaEitherT = do
  e <- lift $ Just 2
  return (e + 1)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a
  
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left a) = f a
either f g (Right a) = g a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g e = runEitherT e >>= either f g

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . (Right <$>)

-- StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor f) => Functor (StateT e f) where
  fmap f (StateT fa) = StateT $ \ s -> (fmap . applyfst) f (fa s)
    where applyfst f (a, s) = (f a , s)

instance (Monad m) => Applicative (StateT e m) where
  pure a = StateT $ \ s -> pure (a, s)
  (<*>) :: Monad m => StateT e m (a -> b) -> StateT e m a -> StateT e m b
  StateT h <*> StateT g = StateT $ \ s -> do
    (f, s') <- h s
    (a, s'') <- g s'
    return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  (>>=) :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT h >>= g = StateT $ \ s -> do
    (a, s') <- h s
    runStateT (g a) s'

putT :: Applicative m => s -> StateT s m ()
putT s = StateT (\ _ -> pure ((), s))

getT :: Applicative m => StateT s m s
getT = StateT (\s -> pure (s,s))

instance MonadTrans (StateT s) where
  lift m = StateT $ \ s -> do
    a <- m
    return (a, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: MonadIO m => IO a -> StateT s m a
  liftIO ioa = StateT $ \ s -> (,s) <$> liftIO ioa

-- Ejemplo Monada con estado con error
monadExampleStateTMaybe :: StateT Int Maybe Int
monadExampleStateTMaybe = do
  putT 1
  s <- getT
  res <- lift $ Just 1
  lift Nothing -- some error
  putT 20
  return s

-- runStateT monadExampleStateTMaybe 3  
  
    
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure b = Reader (\ _ -> b)
  (Reader f) <*> (Reader g) = Reader (\ a -> f a (g a))

instance Monad (Reader r) where
  (>>=) :: Reader a b -> (b -> Reader a c) -> Reader a c
  Reader f >>= g = Reader (\ a -> runReader (g (f a)) a)


{-
And now we’ll make a function similar to some we’ve seen before that
lifts a boolean function over two partially-applied functions:
-}

boltA :: Integer -> Bool
-- use &&, >3, <8
boltA = (&&) . (>3) <*> (<8)

boltM :: Integer -> Bool
boltM = do
  a <- (>3)
  b <- (<8)
  return (a && b)

-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
                                              
test2 = sequenceA [(>3), (<8), even] 7  
  

{-

You tend to see ReaderT, not Reader
Reader rarely stands alone. Usually it’s one Monad in a stack of multi-
ple types providing a Monad instance such as with a web application
that uses Reader to give you access to context about the HTTP request.
When used in that fashion, it’s a monad transformer and we put a letter
T after the type to indicate when we’re using it as such, so you’ll usually
see ReaderT in production Haskell code rather than Reader.

-}

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
  













