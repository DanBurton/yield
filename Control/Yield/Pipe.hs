{-# LANGUAGE DataKinds #-}

module Control.Yield.Pipe where

import Control.Yield
import Control.Yield.Proxy

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative

newtype Pipe r m a b
  = Pipe { unPipe :: Proxy r m '((),b) '((),a) }

type PipeM a b m r = Producing b () (Producing () a m) r

instance (Monad m) => Category (Pipe r m) where
  id = Pipe idProxy
  Pipe p1 . Pipe p2 = Pipe (p1 =$= p2)

instance (Monad m) => Functor (Pipe r m a) where
  fmap f (Pipe (Proxy k)) = Pipe (Proxy (fmap f k))

instance (Monad m) => Applicative (Pipe r m a) where
  pure x = Pipe (Proxy (pure x))
  Pipe (Proxy kf) <*> Pipe (Proxy kx) = Pipe (Proxy (kf <*> kx))

await :: (Monad m) => PipeM a b m a
await = request ()

pipe :: PipeM a b m r -> Pipe r m a b
pipe p = Pipe $ proxyC $ const p

