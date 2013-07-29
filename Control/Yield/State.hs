
module Control.Yield.State where

import Control.Yield
import Control.Monad
import Control.Monad.Fix

data P i o r
  = Stop r
  | Step o (i -> P i o r)

applying :: (o -> i) -> P i o r -> r
applying f = go where
  go (Stop r) = r
  go (Step o k) = go (k (f o))

step :: o -> P i o i
step o = Step o Stop

newStep :: (o -> P i' o' i) -> P i o r -> P i' o' r
newStep f = go where
  go (Stop r) = Stop r
  go (Step o k) = f o >>= go . k

newtype O i o = O { unO :: P i o i }

instance Monad (P i o) where
  return = Stop
  Stop x   >>= f = f x
  Step o k >>= f = Step o ((>>= f) . k)

instance Monad (O i) where
  return o = O (step o)
  o >>= f = O $ newStep (unO . f) (unO o)

instance MonadFix (O i) where
  -- mfix :: (o -> O i o) -> O i o
  mfix f = let m = f (stepped m) in m where
    stepped (O (Stop _)) = error "mfix O: Stop"
    stepped (O (Step o _)) = o

data StateCommand s
  = Get
  | Put s

stateI :: (Monad m) => s -> Consuming r m (StateCommand s) s
stateI s0 = delay (go s0) where
    go s = stateStep s >>= go

stateStep :: (Monad m) => s -> Producing s (StateCommand s) m s
stateStep s = yield s >>= \command -> case command of
  Get -> return s
  Put s' -> return s'

exped :: (MonadFix m) => Consuming r m i o -> Producing o i m r
exped (Consuming k) = mfixP (yield >=> k)

mfixP :: (MonadFix m) => (o -> Producing o i m r) -> Producing o i m r
mfixP = undefined

stateIR :: (MonadFix m) => Producing s (StateCommand s) m r
stateIR = go where
  go = mfixP stateStep >> go

