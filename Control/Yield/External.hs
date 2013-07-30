
module Control.Yield.External (
  Producing,
  fromStep,
  resume,
  Consuming(Consuming, provide),
  ProducerState(Produced, Done),
  Resumable,
  ) where

-- import Control.Yield.Flat
-- import Control.Yield.Cont
import Control.Yield.Internal
  ( Producing,
    fromStep,
    resume,
    Consuming(Consuming, provide),
    ProducerState(Produced, Done),
  )

type Resumable b a m r r' = Either
  (r, ProducerState b a m r')
  (ProducerState a b m r, r')

