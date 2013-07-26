{-# LANGUAGE DataKinds #-}

module Control.Yield.Pipe where

import Control.Yield
import Control.Yield.Proxy

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Monoid
import Util

import Control.Monad.Trans.Class

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


type PullPipe = Pipe
newtype PushPipe r m a b
  = PushPipe { unPushPipe :: a -> PipeM a b m r }


fromPushProxy :: (Monad m)
  => PushProxy r m '((),b) '((),a)
  -> PushPipe r m a b
fromPushProxy (PushProxy p) = PushPipe p

toPushProxy :: (Monad m)
  => PushPipe r m a b
  -> PushProxy r m '((),b) '((),a)
toPushProxy (PushPipe p) = PushProxy p

pullToPushPipe :: (Monad m) => PullPipe r m a b -> PushPipe r m a b
pullToPushPipe (Pipe (Proxy p)) = undefined -- TODO


pushToPullPipe :: (Monad m) => PushPipe r m a b -> a -> PullPipe r m a b
pushToPullPipe (PushPipe p) a = pipe $ p a

-- Producing () a (Producing b () m) r
-- fromProxy :: Proxy r m '((),a) '((),b) -> PushPipe r m a b
-- fromProxy (Proxy (Consuming k)) = PushPipe k

data StateCommand s
  = Get
  | Put s

stateI :: (Monad m) => s -> Consuming r m (StateCommand s) s
stateI s0 = delay (go s0) where
  go s = yield s >>= \m -> case m of
    Get -> go s
    Put s' -> go s'

instance (Monad m) => Category (PushPipe r m) where
  id = fromPushProxy idPush
  p1 . p2 = fromPushProxy (toPushProxy p2 #.# toPushProxy p1)

infixl 1 &
(&) :: a -> (a -> b) -> b
(&) = flip ($)


access2 :: (Monad m)
  => Producing o i (Producing o' i' (Producing o'' i'' m)) r
  -> Producing o'' i'' (Producing o i (Producing o' i' m)) r
access2 = commute . hoist commute
-- id = id
-- commute . commute = id
-- access2 . access2 . access2 = id


replaceAwait :: (Monad m, MonadTrans t, Monad (t m))
  => Producing o () (t m) i
  -> PipeM i o m r
  -> Producing o () (t m) r
replaceAwait await' = replaceRequest (const await')
-- replaceAwait await x = x

awaitingTo :: (Monad m)
  => Producing o () m i
  -> PipeM i o m r
  -> Producing o () m r
awaitingTo a = requestingTo (const a)
-- awaitingTo await = hoist squash

instance (Monad m) => Arrow (PushPipe r m) where
  arr f = PushPipe $ foreverK $ \a -> yield (f a) >> await
  first (PushPipe p0) = PushPipe $ \(a,d) ->
    p0 a
      & insert2
      & replaceYield yield'
      & replaceAwait await'
      & access2
      $- stateI d
    where
      yield' b = do
        d <- lift $ lift $ yield Get
        yield (b,d)
      await' = do
        (a,d) <- await
        lift $ lift $ yield (Put d)
        return a

instance (Monad m) => Functor (PushPipe r m a) where
  fmap f p = arr f . p

instance (Monad m) => Applicative (PushPipe r m a) where
  pure z = arr (const z)
  af <*> ax = fmap (uncurry ($)) (af &&& ax)

instance (Monad m, Monoid r) => ArrowZero (PushPipe r m) where
  zeroArrow = PushPipe $ \_ -> return mempty

instance (Monad m, Monoid r) => ArrowPlus (PushPipe r m) where
  -- a b c -> a b c -> a b c
  p1 <+> p2 = to (from p1 <+> from p2) where
    from (PushPipe p) = unProxy $ pushToPull $ PushProxy p
    to c = PushPipe $ unPullProxy $ pullToPush $ Proxy c

instance (Monad m) => ArrowChoice (PushPipe r m) where
  -- a b c -> a (Either b d) (Either c d)
  left (PushPipe p) = PushPipe (skipToB >=> p') where
    p' = const await' \$\ p /$/ yield'
    skipToB (Left b) = return b
    skipToB (Right d) = yield (Right d) >> await'
    await' = await >>= skipToB
    yield' = yield . Left


instance (Monad m) => ArrowApply (PushPipe r m) where
  -- a (a b c, b) c
  app = undefined

instance (Monad m) => ArrowLoop (PushPipe r m) where
  -- a (b, d) (c, d) -> a b c
  loop p = undefined
