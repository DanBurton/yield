{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- coverage condition...
{-# LANGUAGE Rank2Types #-}

module Control.Yield (
  -- * Types
  Producing(..),
  Consuming(..),
  ProducerState(..),

  -- * Basic introduction and elimination
  yield,
  pfold,

  -- * Handy conversion functions
  delay,
  overConsumption,
  overProduction,
  afterYielding,
  
  -- * Meaningful specializations of pfold
  replaceYield,
  foreverYield,
  yieldingTo,
  foreach,
  ($//),
  (/$/),

  -- * Connecting computations
  ($-),
  ($~),
  connectResume,

  -- * Misc
  yieldEach,
  echo,
  echo_,
  voidC,

  -- * In the category of monads
  hoist,
  squash,
  selfConnection,
  inputThrough,
  through,

  -- * Manipulating layers of Producing
  insert0,
  insert1,
  insert2,
  commute,
  ) where

import Util

import Control.Arrow
import Control.Applicative
import Control.Category
import Data.Monoid
import Data.Foldable

import Prelude hiding ((.), id, mapM_)
import Control.Monad hiding (mapM_)

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.Cont.Class
import Control.Monad.Trans.Class

import Control.Monad.Fix

-- Types
---------------------------------------------------------------

newtype Producing o i m r
  = Producing { resume :: m (ProducerState o i m r) }


data ProducerState o i m r
  = Produced o (Consuming r m i o)
  | Done r


newtype Consuming r m i o
  = Consuming { provide :: i -> Producing o i m r }


type Resumable o i m r r'
  = Either (ProducerState i o m r, r') (ProducerState o i m r', r)


-- Basic introduction and elimination
----------------------------------------------------------------

yield :: (Monad m) => o -> Producing o i m i
yield o = Producing $ return $ Produced o $ Consuming return


-- The first argument is just a specialization of
-- (forall x. m x -> m' x)
pfold :: (Monad n)
  => (forall x. m x -> n x) -- (m (ProducerState o i m r) -> m' (ProducerState o i m r))
  -> (o -> n i) -> Producing o i m r -> n r
pfold morph yield' = go where
  go p = morph (resume p) >>= \s -> case s of
    Done r -> return r
    Produced o k -> yield' o >>= go . provide k
-- pfold lift yield x = x
-- pfold f' k' ∘ pfold f k = pfold (f' ∘ f) (k' ∘ k)


-- Handy conversion functions
-----------------------------------------------------------------

delay :: (Monad m) => Producing o i m r -> Consuming r m i o
delay p = Consuming $ \i -> lift (resume p) >>= \s -> case s of
  Done r -> return r
  Produced o k -> yield o >>= provide (delay $ provide k i)

{- ???
expedite :: (MonadFix m) => Consuming r m i o -> Producing o i m r
expedite = undefined
-}

overProduction ::
    (Producing o i m r -> Producing o' i m' r')
  -> Consuming r m i o -> Consuming r' m' i o'
overProduction f k = Consuming (f . provide k)
-- overProduction f ∘ overProduction g = overProduction (f ∘ g)


overConsumption :: (Monad m)
  => (Consuming r m i o -> Consuming r m i' o)
  ->  Producing o i m r -> Producing o i' m r
overConsumption f p = Producing $ resume p >>= \s -> case s of
  Done r -> return $ Done r
  Produced o k -> return $ Produced o (f k)
-- overConsumption f ∘ overConsumption g = overConsumption (f ∘ g)


afterYielding :: (Monad m)
  => (Producing o i m r -> Producing o i m r)
  ->  Producing o i m r -> Producing o i m r
afterYielding = overConsumption . overProduction
-- afterYielding f ∘ afterYielding g = afterYielding (f ∘ g)


-- Meaningful specializations of pfold
--------------------------------------------------------------------

replaceYield :: (Monad m, MonadTrans t, Monad (t m))
  => (o -> t m i) -> Producing o i m r -> t m r
replaceYield = pfold lift
-- replaceYield yield x = x
-- replaceYield f' ∘ replaceYield f = replaceYield (f' <=< liftBase ∘ f)
-- replaceYield return =?= lift ∘ selfConnect

-- example
-- swap a = get >>= \b -> put a >> return b
-- replaceYield swap :: (MonadBase m m', MonadState i m') =>
--   Producing i i m r -> m' r


foreverYield :: (Monad m) => (i -> m o) -> Consuming r m i o
foreverYield k = replaceYield (lift . k >=> yield) `overProduction` id
-- foreverYield return = id ∷ Consuming r m i i


yieldingTo :: (Monad m) => (o -> m i) -> Producing o i m r -> m r
yieldingTo = pfold id
-- if the type system were powerful enough,
-- yieldingTo = replaceYield


foreach :: (Monad m) => Producing o i m r -> (o -> m i) -> m r
foreach = flip yieldingTo
-- p `foreach` k = p $- foreverYield k


($//) :: (Monad m, MonadTrans t, Monad (t m))
  => Producing o i m r -> (o -> t m i) -> t m r
p $// k = replaceYield k p


infixr 4 /$/
-- composable replaceYield with monoid laws
(/$/) :: (Monad m, MonadTrans t, Monad (t m))
  => (a -> Producing o i m r) -> (o -> t m i)
  -> (a -> t m r)
k1 /$/ k2 = \i -> k1 i $// k2
-- yield /$/ x = x
-- x /$/ yield = x
-- a /$/ (b />/ c) = (a />/ b) />/ c

-- return /$/ x = return
-- lift   /$/ x = lift
-- x /$/ return =?= lift . selfConnect


-- Connecting computations
------------------------------------------------------------------

infixl 0 $-
($-) :: (Monad m) => Producing a b m r -> Consuming r m a b -> m r
p $- c = resume p >>= \s -> case s of
  Done r -> return r
  Produced o k -> provide c o $- k


infixl 0 $~
($~) :: (Monad m) => Producing a b m r -> (a -> Producing b a m r) -> m r
p $~ k = p $- Consuming k


connectResume :: (Monad m) => Consuming r m b a -> Consuming r' m a b -> b -> m (Resumable b a m r r')
connectResume k1 k2 = \b -> resume (provide k1 b) >>= \s -> case s of
  Done r -> return (Right (Produced b k2, r))
  (Produced a k1') -> resume (provide k2 a) >>= \s2 -> case s2 of
    Done r' -> return (Left (Produced a k1', r'))
    Produced b' k2' -> connectResume k1' k2' b'


-- Misc
------------------------------------------------------------------

yieldEach :: (Monad m, Foldable f) => f a -> Producing a b m ()
yieldEach = mapM_ yield


echo :: (Monad m) => Int -> Consuming a m a a
echo n | n >= 0 = Consuming $ replicateK n yield
echo _ = Consuming $ \_ -> fail "echo requires a nonnegative argument"


echo_ :: (Monad m) => Int -> Consuming () m a a
echo_ = voidC . echo


voidC :: (Monad m) => Consuming r m i o -> Consuming () m i o
voidC = overProduction void


-- As a functor in the category of monads
------------------------------------------------------------------

-- hoist = map
hoist :: (Monad n)
  {- => (  m  (ProducerState o i m r)
     -> m' (ProducerState o i m r)) -}
  => (forall x. m x -> n x)
  -> Producing o i m r -> Producing o i n r
hoist f = pfold (lift . f) yield
-- hoist id x = x


-- As a monad in the category of monads
-------------------------------------------------------------------

-- squash = join
squash :: (Monad m)
  => Producing o i (Producing o i m) r
  -> Producing o i m r
squash = yieldingTo yield
-- flatten (insert0 x) = x
-- flatten (insert1 x) = x
-- flatten (insert2 x) = insert1 (flatten x)


-- embed = bind
embed :: (Monad n)
  => (forall x. m x -> Producing i o n x)
  -> Producing i o m r -> Producing i o n r
embed f m = squash (hoist f m)


-- As an indexed comonad in the category of monads
-------------------------------------------------

-- selfConnection = copoint
selfConnection :: (Monad m) => Producing i i m r -> m r
selfConnection = yieldingTo return
-- selfConnection (flatten x) =?= selfConnection (hoist selfConnection x)


-- inputThrough = extend
inputThrough :: (Monad n, Monad m)
  => (forall x. Producing j k m x -> n x)
  -> Producing i k m r -> Producing i j n r
inputThrough morph = go where
    go p = Producing $ morph $ liftM map' (lift (resume p))
    map' (Done r) = Done r
    map' (Produced i consuming) = Produced i $ Consuming $ \j -> do
      k <- lift (jToK j)
      go (provide consuming k)
    jToK j = morph (yield j)
-- inputThrough selfConnection x = x


-- through = duplicate
through :: (Monad m)
  => Producing o                i m  r
  -> Producing o x (Producing x i m) r
through = inputThrough id
-- selfConection (through x) = x


-- Manipulating layers of Producing
------------------------------------------------------------

insert0 :: (Monad m, MonadTrans t, Monad (t m)) => m r -> t m r
insert0 = lift


insert1 :: (Monad m, MonadTrans t, Monad (t m))
  => Producing o i m r
  -> Producing o i (t m) r
insert1 = hoist insert0


insert2 :: (Monad m, MonadTrans t, Monad (t m))
  => Producing o i (Producing o' i' m) r
  -> Producing o i (Producing o' i' (t m)) r
insert2 = hoist insert1


commute :: (Monad m)
  => Producing a b (Producing c d m) r
  -> Producing c d (Producing a b m) r
commute p = p' $- idP where
  p' = insert2 p
  idP = insert1 `overProduction` idProxy
  idProxy = foreverYield yield
       -- = Consuming $ fix ((lift . yield >=> yield) >=>)
-- commute (commute x) =?= x
-- commute (lift $ yield x) = yield x
-- commute (yield x) = lift $ yield x
-- commute (return x) = return x
-- commute ∘ (f >=> g) = commute ∘ f >=> commute ∘ g


-- Instances
----------------------------------------------------------------

-- a common pattern in implementing instances
rewrap :: (MonadTrans t, Monad m) =>
  Consuming r m i o -> i -> t m (ProducerState o i m r)
rewrap p a = lift (resume (provide p a))


-- Producing instances
---------------------------------

instance (Monad m) => Functor (Producing o i m) where
   fmap = liftM


instance (Monad m) => Applicative (Producing o i m) where
   pure = return
   (<*>) = ap


instance (Monad m) => Monad (Producing o i m) where
   return x = lift (return x)
   p >>= f = Producing $ resume p >>= \s -> case s of
     Done r -> resume (f r)
     Produced o k -> return $ Produced o $ Consuming (provide k >=> f)
   fail = lift . fail


instance MonadTrans (Producing o i) where
  lift m = Producing (liftM Done m)


instance (Monad m, MonadIO m) => MonadIO (Producing o i m) where
  liftIO = lift . liftIO


{- ???
instance (MonadFix m) => MonadFix (Producing o i m) where
  mfix = undefined
-}

-- mtl instances for Producing
-----------------------------------

instance (Monad m, MonadReader r m) => MonadReader r (Producing o i m) where
  ask = lift ask
  local f = hoist (local f)
  reader = lift . reader


instance (Monad m, MonadState r m) => MonadState r (Producing o i m) where
  get = lift get
  put = lift . put
  state = lift . state


instance (Monad m, Monoid w, MonadWriter w m) => MonadWriter w (Producing o i m) where
  writer = lift . writer
  tell = lift . tell
  listen m = Producing $ listen (resume m) >>= \(s, w) -> case s of
    Done r -> return $ Done (r, w)
    Produced o k ->
      let k' = liftM (second (w <>)) . listen . provide k
      in return $ Produced o (Consuming k')
  -- not sure if this is legit
  pass m = Producing $ pass $ resume m >>= \s -> case s of
    Done (r, f) -> return (Done r, f)
    Produced o k ->
      let k' = pass . provide k
      in return (Produced o (Consuming k'), id)

instance (Monad m, MonadError e m) => MonadError e (Producing o i m) where
  throwError = lift . throwError
  p `catchError` h = lift (safely (resume p)) >>= \s -> case s of
    Left err -> h err
    Right (Done r) -> return r
    Right (Produced o k) -> do
      i <- yield o
      provide k i `catchError` h
    where
      safely m = liftM Right m `catchError` \e -> return (Left e)

instance (Monad m, MonadCont m) => MonadCont (Producing o i m) where
  callCC f = Producing $ callCC $ \k ->
    resume (f $ lift . k . Done)

-- Consuming instances
--------------------------------------

instance (Monad m) => Functor (Consuming r m a) where
  fmap f = overProduction $ replaceYield (yield . f)


instance (Monad m) => Applicative (Consuming r m a) where
  pure a = arr (const a)
  kf <*> kx = Consuming $ \a -> rewrap kf a >>= \s -> case s of
    Done r -> return r
    Produced f kf' -> rewrap kx a >>= \s -> case s of
      Done r -> return r
      Produced x kx' ->
        yield (f x) >>= provide (kf' <*> kx')


instance (Monad m) => Category (Consuming r m) where
  id = Consuming $ let go = yield >=> go in go
  k2 . k1 = Consuming $ rewrap k1 >=> \s -> case s of
    Done r -> return r
    Produced b k1' -> rewrap k2 b >>= \s2 -> case s2 of
      Done r -> return r
      Produced c k2' ->
        yield c >>= provide (k2' . k1')


instance (Monad m) => Arrow (Consuming r m) where
  arr f = Consuming go where go = yield . f >=> go
  first k = Consuming $ \(b, d) -> rewrap k b >>= \s -> case s of
    Done r -> return r
    Produced c k' -> yield (c, d) >>= provide (first k')


instance (Monad m, Monoid r) => ArrowZero (Consuming r m) where
  zeroArrow = Consuming $ \_ -> return mempty


instance (Monad m, Monoid r) => ArrowPlus (Consuming r m) where
  k1 <+> k2 = Consuming $ \i -> rewrap k1 i >>= \s -> case s of
    Done r -> liftM (r <>) (provide k2 i)
    Produced o k1' -> yield o >>= provide (k1' <+> k2)


instance (Monad m) => ArrowChoice (Consuming r m) where
  left k = Consuming go where
    go = \e -> case e of
      Right d -> yield (Right d) >>= go
      Left b -> rewrap k b >>= \s -> case s of
        Done r -> return r
        Produced c k' -> yield (Left c) >>= provide (left k')


{-
-- left =?= leftApp
instance (Monad m) => ArrowApply (Consuming r m) where
  app = Consuming go where
    go = \(kf, b) -> rewrap kf b >>= \s -> case s of
      Done r -> return r
      Produced c _ -> yield c >>= go
      -- ignoring k' makes me weary
-}
