{-# LANGUAGE CPP #-}

#define AMonad(m) Applicative m, Monad m

module Control.Yield.Category (
  YieldC(..),
  idY,
  (/>/),
  )where

import Util
import Control.Yield
import Control.Applicative

newtype YieldC m a b = YieldC { unYieldC ::
  Fst a -> Producing (Fst b) (Snd b) m (Snd a) }

idY :: (AMonad(m)) => YieldC m a a
idY = YieldC yield

(/>/) :: (AMonad(m)) => YieldC m a b -> YieldC m b c -> YieldC m a c
YieldC k1 />/ YieldC k2 = YieldC (k1 /$/ k2)

{-
instance (AMonad(m)) => Category (YieldC m) where
  id = idY
  (.) = flip (/>/)
-}
