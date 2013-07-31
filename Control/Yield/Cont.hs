{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Yield.Cont where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class

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


