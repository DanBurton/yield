
module Control.Yield.Internal (
    Producing,
    Consuming(Consuming, provide),
    ProducerState(Produced, Done),
    resume,
    fromStep,
  ) where

-- | The Producing monad transformer
newtype Producing o i m r
  = Producing { unProducing :: m (ProducerState o i m r) }


-- | A data structure that describes
-- the state of a Producing computation
-- once it chooses to pause.
data ProducerState o i m r
  = Produced o (Consuming r m i o)
    -- ^ The computation isn't Done yet,
    -- but it has yielded control by producing an `o`.
    -- It is now waiting to be `provide`d with an `i`.
  | Done r
    -- ^ The computation has terminated with
    -- a final result `r`.


-- | A computation in Consuming mode
-- is blocked on its interface, waiting for
-- an input `i`. Consuming is a very thin newtype,
-- but it allows us to declare meaningful
-- Category and Arrow instances.
newtype Consuming r m i o
  = Consuming
    { provide :: i -> Producing o i m r
      -- ^ Provide the blocked computation with the input it is
      -- waiting for, so that it can be `resume`d.
    }


-- | The reverse of resume.
-- Given a computation that results in a ProducerState,
-- we can repackage it as a Producing computation.
-- 
-- > fromStep ∘ resume ≡ id
-- > resume ∘ fromStep ≡ id
fromStep :: (Monad m) => m (ProducerState o i m r) -> Producing o i m r
fromStep = Producing
{-# INLINE fromStep #-}


-- | Step a Producing computation until it has Produced
-- a value, or is Done.
resume :: (Monad m) => Producing o i m r -> m (ProducerState o i m r)
resume = unProducing
{-# INLINE resume #-}

