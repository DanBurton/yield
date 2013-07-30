

module Control.Yield.Flat where

import Control.Monad (liftM)

data Producing o i m r
  = Yield o (i -> Producing o i m r)
  | M (m (Producing o i m r))
  | R r

newtype Consuming r m i o
  = Consuming { provide :: i -> Producing o i m r }

data ProducerState o i m r
  = Produced o (Consuming r m i o)
  | Done r

fromStep :: Monad m => m (ProducerState o i m r) -> Producing o i m r
fromStep m = M $ liftM f m where
  f (Done r) = R r
  f (Produced o (Consuming k)) = Yield o k

resume :: Monad m => Producing o i m r -> m (ProducerState o i m r)
resume (M m) = m >>= resume
resume (R r) = return (Done r)
resume (Yield o k) = return (Produced o (Consuming k))

