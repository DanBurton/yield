{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Yield.Cont where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class

import Control.Monad (liftM)

infix 2 `provide`
infix 1 `using`

newtype Producing o i m a
  = Producing { using :: forall r. (o -> ContT r m i) -> ContT r m a }

newtype Consuming a m i o
  = Consuming { provide :: i -> Producing o i m a }

data ProducerState o i m a
  = Produced o (Consuming a m i o)
  | Done a

fromStep :: Monad m => m (ProducerState o i m a) -> Producing o i m a
fromStep p = Producing $ \yield' -> lift p >>= \s -> case s of
  Done a -> return a
  Produced o k -> do
    i <- yield' o
    k `provide` i `using` yield'

resume :: Monad m => Producing o i m a -> m (ProducerState o i m a)
resume p = runContT (p `using` yield') (return . Done) where
  -- yield' :: o -> ContT (ProducerState o i m a) m i
  yield' o = ContT $ \k -> return $ Produced o $ Consuming (fromStep . k)


-- Alternate implementations
---------------------------------------------------------------------------

yield :: Monad m => o -> Producing o i m i
yield o = Producing ($ o)


pfold :: forall m n o i r.
  (Monad m, Monad n)
  => (forall x. m x -> n x)
  -> (o -> n i) -> Producing o i m r -> n r
pfold morph yield' p = runContT c return where
  c = hoist morph p `using` lift . yield'


-- ew! Want to implement this more... continuation-y
hoist :: (Monad m, Monad n)
  => (forall x. m x -> n x)
  -> Producing o i m r -> Producing o i n r
hoist morph = go where
  go p = fromStep $ liftM convert (morph (resume p)) where
    convert (Done r) = Done r
    convert (Produced o k) = Produced o (Consuming (go . provide k))

