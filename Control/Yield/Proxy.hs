{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE CPP #-}


#define AMonad(m) Applicative m, Monad m

module Control.Yield.Proxy where

import Util
import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Yield
import Control.Monad.Trans.Class

-- d = (dI, dO)
-- u = (uO, uI)

newtype Proxy r m d u = Proxy
  { asConsuming :: Consuming r
                   (Producing (Fst u) (Snd u)  m)
                   (Fst d) (Snd d) }

type ProxyM d u m r =
  Producing (Snd d) (Fst d)
  (Producing (Fst u) (Snd u) m) r

request :: (AMonad(m)) =>
  uO -> Producing dO dI (Producing uO uI m) uI
request o = lift (yield o)

proxyC :: (Fst d ~ dI, Snd d ~ dO, Fst u ~ uO, Snd u ~ uI) =>
  (dI -> Producing dO dI (Producing uO uI m) r) -> Proxy r m d u
proxyC = Proxy . Consuming

idProxy :: (AMonad(m)) => Proxy r m a a
idProxy = proxyC $ foreverK (request >=> yield)

infixl 1 $=
infixr 2 =-
infixr 2 =$=

(=$=) :: (AMonad(m)) => Proxy r m a b -> Proxy r m b c -> Proxy r m a c
Proxy p1 =$= Proxy p2 = proxyC $ \a ->
  insert2 (commute (provide p1 a)) $- overProduction insert1 p2

(=.=) :: (AMonad(m)) => Proxy r m b c -> Proxy r m a b -> Proxy r m a c
(=.=) = flip (=$=)

($=) :: (AMonad(m)) =>
  Producing a b m r -> Proxy r m '(a,b) '(c,d) -> Producing c d m r
prod $= Proxy prox = insert1 prod $- prox

(=-) :: (AMonad(m)) =>
  Proxy r m '(a,b) '(c,d) -> Consuming r m c d -> Consuming r m a b
Proxy p =- c = Consuming $ \a' ->
  commute (provide p a') $- overProduction insert1 c

promote :: (AMonad(m)) => Consuming r m a b -> Proxy r m '(c,b) '(c,a)
promote k = proxyC $ \c -> do
  a <- request c
  through (provide k a)

{-
instance (AMonad(m)) => Category (Proxy r m) where
  id = idProxy
  (.) = (=.=)
-}

