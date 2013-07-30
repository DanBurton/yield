
module Control.Yield.Internal where


newtype Producing o i m r
  = Producing { unProducing :: m (ProducerState o i m r) }


data ProducerState o i m r
  = Produced o (Consuming r m i o)
  | Done r


newtype Consuming r m i o
  = Consuming { provide :: i -> Producing o i m r }


fromStep :: (Monad m) => m (ProducerState o i m r) -> Producing o i m r
fromStep = Producing
{-# INLINE fromStep #-}


resume :: (Monad m) => Producing o i m r -> m (ProducerState o i m r)
resume = unProducing
{-# INLINE resume #-}

