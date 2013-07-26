{-# LANGUAGE TypeFamilies, DataKinds #-}

module Control.Yield.Proxy where

import Util
import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Yield
import Control.Monad.Trans.Class


type ProxyM d u m r =
  Producing (Snd d) (Fst d)
  (Producing (Fst u) (Snd u) m) r


-- d = (dI, dO)
-- u = (uO, uI)
newtype Proxy r m d u = Proxy
  (Consuming r (Producing (Fst u) (Snd u) m)
   (Fst d) (Snd d))

-- blocked on (d)ownstream
type PullProxy = Proxy

-- blocked on (u)pstream
newtype PushProxy r m d u = PushProxy
  { unPull :: (Snd u) -> ProxyM d u m r }


reflect :: Monad m
  => (x -> ProxyM '(dI,dO) '(uO,uI) m r)
  ->  x -> ProxyM '(uI,uO) '(dO,dI) m r
reflect f x = commute (f x)


pullToPush :: Monad m
  => PullProxy r m '(dI,dO) '(uO,uI)
  -> PushProxy r m '(uI,uO) '(dO,dI)
pullToPush (Proxy (Consuming k)) = PushProxy (reflect k)


pushToPull :: Monad m
  => PushProxy r m '(dI,dO) '(uO,uI)
  -> PullProxy r m '(uI,uO) '(dO,dI)
pushToPull (PushProxy k) = proxyC (reflect k)



request :: (Monad m) =>
  uO -> ProxyM '(dO,dI) '(uO,uI) m uI
request o = lift (yield o)

replaceRequest :: (Monad m, MonadTrans t, Monad (t m))
  => (uO -> Producing dO dI (t m) uI)
  -> ProxyM '(dI,dO) '(uO,uI) m r
  -> Producing dO dI (t m) r
replaceRequest req' p = yieldingTo req' (insert2 (commute p))
-- replaceRequest request x = x

(\$\) :: (Monad m, MonadTrans t, Monad (t m))
  => (uO -> Producing dO dI (t m) uI)
  -> (x -> ProxyM '(dI,dO) '(uO,uI) m r)
  -> x -> Producing dO dI (t m) r
req' \$\ p = \x -> replaceRequest req' (p x)

requestingTo :: (Monad m)
  => (uO -> Producing dO dI m uI)
  -> Producing dO dI (Producing uO uI m) r
  -> Producing dO dI m r
requestingTo req' p = yieldingTo req' (commute p)
-- requestingTo request x = hoist squash x

proxyC :: ((Fst d) -> ProxyM d u m r) -> Proxy r m d u
proxyC = Proxy . Consuming

idProxy :: (Monad m) => Proxy r m a a
idProxy = proxyC $ foreverK (request >=> yield)

infixl 1 $=
infixr 2 =-
infixr 2 =$=

(=$=) :: (Monad m) => Proxy r m a b -> Proxy r m b c -> Proxy r m a c
Proxy p1 =$= Proxy p2 = proxyC $ \a ->
  insert2 (commute (provide p1 a)) $- overProduction insert1 p2

(=.=) :: (Monad m) => Proxy r m b c -> Proxy r m a b -> Proxy r m a c
(=.=) = flip (=$=)

($=) :: (Monad m) =>
  Producing a b m r -> Proxy r m '(a,b) '(c,d) -> Producing c d m r
prod $= Proxy prox = insert1 prod $- prox

(=-) :: (Monad m) =>
  Proxy r m '(a,b) '(c,d) -> Consuming r m c d -> Consuming r m a b
Proxy p =- c = Consuming $ \a' ->
  commute (provide p a') $- overProduction insert1 c

promote :: (Monad m) => Consuming r m a b -> Proxy r m '(c,b) '(c,a)
promote k = proxyC $ \c -> do
  a <- request c
  through (provide k a)

(#$) :: (Monad m) =>
  PushProxy r m '(a,b) '(c,d) -> Producing d c m r -> Producing b a m r
prox #$ prod = prod $= pushToPull prox

(-#) :: (Monad m) =>
  Consuming r m b a -> PushProxy r m '(a,b) '(c,d) -> Consuming r m d c
c -# prox = pushToPull prox =- c

-- The type system is too finnicky to express these
-- in adequately general terms
(#.#) :: (Monad m)
  => PushProxy r m '(b,b') '(c,c')
  -> PushProxy r m '(a,a') '(b,b')
  -> PushProxy r m '(a,a') '(c,c')
p1 #.# p2 = pullToPush (pushToPull p1 =$= pushToPull p2)

idPush :: (Monad m)
  => PushProxy r m '(a,b) '(a,b)
idPush = pullToPush idProxy


