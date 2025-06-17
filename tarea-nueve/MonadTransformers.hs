-- Denise Souberville 223427
-- Bruno Monfort 173280
-- Nicolás Capellino 272778



module MonadTransformers where

import Control.Monad.Trans.Maybe

import Data.Foldable
import Prelude 
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }


--Write the code
{- 1. rDec is a function that should get its argument in the context of
Reader and return a value decremented by one.
rDec :: Num a => Reader a a
rDec = undefined
Prelude> import Control.Monad.Trans.Reader
Prelude> runReader rDec 1
0
Prelude> fmap (runReader rDec) [1..10]
[0,1,2,3,4,5,6,7,8,9]
Note that “Reader” from transformers is actually ReaderT of
Identity and that runReader is a convenience function throwing
away the meaningless structure for you. Play with runReaderT
if it tickles your nondescript furry red puppet.
-}
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure b = Reader (\ _ -> b)
  (Reader f) <*> (Reader g) = Reader (\ a -> f a (g a))

instance Monad (Reader r) where
  (>>=) :: Reader a b -> (b -> Reader a c) -> Reader a c
  Reader f >>= g = Reader (\ a -> runReader (g (f a)) a)
  
rDec :: Num a => Reader a a
rDec = do
    n <- Reader 
    return n - 1


{- 2. Once you have an rDec that works, make it and any inner lambdas
pointfree if that’s not already the case.
-}

{- PRIMER EJERCICIO:3. rShow is show, but in Reader.
rShow :: Show a => ReaderT a Identity String
rShow = undefined
Prelude> runReader rShow 1
"1"
Prelude> fmap (runReader rShow) [1..10]
["1","2","3","4","5","6","7","8","9","10"]
-}

{- 4. Once you have an rShow that works, make it pointfree.
-}


{- 5. rPrintAndInc will first print the input with a greeting, then
return the input incremented by one.
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


{- 6. sPrintIncAccum first prints the input with a greeting, then puts
the incremented input as the new state, and returns the original
input as a String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = undefined
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