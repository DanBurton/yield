
module Control.Yield.State where

import Control.Yield
import Control.Monad
import Control.Monad.Fix


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

