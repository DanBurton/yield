
-- for mtl
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- for hoist and inputThrough
{-# LANGUAGE Rank2Types #-}

module Control.Yield (
  -- * Types
  Producing,
  resume,
  fromStep,
  Consuming(Consuming, provide),
  ProducerState(Produced, Done),
  Resumable,

  -- * Basic introduction and elimination
  yield,
  pfold,

  -- * Handy conversion functions
  delay,
  discardingFirstInput,
  step,
  asNextYieldOf,
  (/>/),
  peeking,
  overConsumption,
  overProduction,
  afterYielding,

  -- * Meaningful specializations of pfold
  replaceYield,
  foreverYield,
  yieldingTo,
  (/$/),

  -- * Connecting computations
  ($-),
  ($~),
  ($$),
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
import Control.Yield.External

import Control.Arrow
import Control.Applicative
import Control.Category
import Data.Monoid
import Data.Foldable

import Prelude hiding ((.), id, mapM_, foldl)
import Control.Monad hiding (mapM_)

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.Cont.Class
import Control.Monad.Trans.Class

import Control.Monad.Fix


-- Basic introduction and elimination
----------------------------------------------------------------

-- | Surrender an o over the interface,
-- waiting for an i in response.
yield :: (Monad m) => o -> Producing o i m i
yield o = fromStep $ return $ Produced o $ Consuming return


-- | A general algorithm for interpreting a Producing computation.
-- 
-- > pfold lift yield ≡ id
-- > pfold f' k' ∘ pfold f k ≡ pfold (f' ∘ f) (k' ∘ k)
pfold :: (Monad m, Monad n)
  => (forall x. m x -> n x) -- (m (ProducerState o i m r) -> m' (ProducerState o i m r))
  -> (o -> n i) -> Producing o i m r -> n r
pfold morph yield' = go where
  go p = morph (resume p) >>= \s -> case s of
    Done r -> return r
    Produced o k -> yield' o >>= go . provide k


-- Handy conversion functions
-----------------------------------------------------------------

-- | Wait for input before you actually need it,
-- delaying production. Each time new input arrives,
-- the previous input is used instead.
delay :: (Monad m) => Producing o i m r -> Consuming r m i o
delay p = Consuming $ \i -> lift (resume p) >>= \s -> case s of
  Done r -> return r
  Produced o k -> yield o >>= provide (delay $ provide k i)

-- | Wait for input before you actually need it,
-- discarding it when it arrives.
discardingFirstInput :: (Monad m) => Producing o i m r -> Consuming r m i o
discardingFirstInput p = Consuming $ \_ -> p


-- | Provide an action to be plugged in at the next yield.
-- All underlying effects up until that point will be run.
-- You will get back either the result r,
-- or a new Producing computation which you can 'resume'
-- just where it left of: right after that yield you just replaced,
-- but before any of the replacement code has run.
-- 
-- > step f (return r) ≡ return (Left r)
-- > step f (yield o >>= k) ≡ return (Right (f o >>= k))
-- > step f (lift m >>= k) ≡ m >>= step f ∘ k
step :: (Monad m) => (o -> Producing o i m i) -> Producing o i m r -> m (Either r (Producing o i m r))
step f p = step' `liftM` resume p where
  step' (Done r) = Left r
  step' (Produced o k) = Right $ f o >>= provide k

-- | Provide an action to replace the next yield.
-- If p does not yield, then @f \`asNextYieldOf\` p ≡ p@.
-- 
-- > asNextYieldOf f (return r) ≡ return r
-- > asNextYieldOf f (yield o >>= k) ≡ f o >>= k
-- > asNextYieldOf f (lift m >>= k) ≡ lift m >>= asNextYieldOf f ∘ k
asNextYieldOf :: (Monad m) => (o -> Producing o i m i)
  -> Producing o i m r -> Producing o i m r
asNextYieldOf f p = lift (resume p) >>= step' where
  step' (Done r) = return r
  step' (Produced o k) = f o >>= provide k

infixl 3 />/

-- | Pronounced \"with next yield\".
-- This is just a flipped `asNextYieldOf`
-- with monoid laws.
-- 
-- > yield />/ f ≡ f
-- > f />/ yield ≡ f
-- > f />/ (g />/ h) ≡ (f />/ g) />/ h
-- > 
-- > return />/ f ≡ return
-- > lift />/ f ≡ lift
(/>/) :: (Monad m)
  => (a -> Producing o i m r)
  -> (o -> Producing o i m i)
  ->  a -> Producing o i m r
(f1 />/ f2) a = f2 `asNextYieldOf` f1 a

-- | Peeking! Look at the next n inputs.
-- 
-- Inside the peeking block, you are guaranteed
-- that your first n yields will correspond in order
-- to the list of inputs you are peeking at.
-- The list of inputs is guaranteed to be of length n,
-- making it convenient to bind all of the inputs
-- with a single pattern match.
-- 
-- After the peeking block, you are guaranteed
-- that yield has been used at least n times,
-- possibly more.
-- 
-- > do r <- peeking 2 $ \[i1, i2] -> do
-- >      The next two yields in here
-- >      will get you i1 and i2 respectively.
-- >      You may yield more if you wish.
-- >      If you yield fewer than 2 times in this block,
-- >      then following this block,
-- >      it will be as though you had yielded exactly 2 times.
-- >    At this point, you are guaranteed that
-- >    at least 2 yields have happened, possibly more.
peeking :: (Monad m) => Int -> ([i] -> Producing () i m r)
           -> Producing () i m r
peeking n k = do
  is <- replicateM n (yield ())
  foldl (\m i -> const (return i) `asNextYieldOf` m) (k is) is



{- ???
expedite :: (MonadFix m) => Consuming r m i o -> Producing o i m r
expedite = undefined
-}

-- | Take a transformation of Producing computations,
-- and apply it to a Consuming computation.
-- 
-- > overProduction id ≡ id
-- > overProduction (f ∘ g) ≡ overProduction f ∘ overProduction g
overProduction ::
    (Producing o i m r -> Producing o' i m' r')
  -> Consuming r m i o -> Consuming r' m' i o'
overProduction f k = Consuming (f . provide k)


-- | Take a transformation of Consuming computations,
-- and apply it to a Producing computation.
-- 
-- > overConsumption id ≡ id
-- > overConsumption (f ∘ g) ≡ overConsumption f ∘ overConsumption g
overConsumption :: (Monad m)
  => (Consuming r m i o -> Consuming r m i' o)
  ->  Producing o i m r -> Producing o i' m r
overConsumption f p = fromStep $ resume p >>= \s -> case s of
  Done r -> return $ Done r
  Produced o k -> return $ Produced o (f k)


-- | Take a transformation of Producing computations,
-- and wait until right after the next yield to apply it.
-- 
-- > afterYielding ≡ overConsumption ∘ overProduction
-- > afterYielding id ≡ id
-- > afterYielding (f ∘ g) ≡ afterYielding f ∘ afterYielding g
afterYielding :: (Monad m)
  => (Producing o i m r -> Producing o i m r)
  ->  Producing o i m r -> Producing o i m r
afterYielding = overConsumption . overProduction



-- Meaningful specializations of pfold
--------------------------------------------------------------------

-- | Replace the Producing o i monad transformer
-- with some other monad transformer, by filling in
-- the yield holes with a given computation from o to i.
-- 
-- 
-- > replaceYield yield ≡ id
-- > replaceYield f' ∘ replaceYield f ≡ replaceYield (f' <=< lift ∘ f)
-- > replaceYield return ≡ lift ∘ selfConnect
replaceYield :: (Monad m, MonadTrans t, Monad (t m))
  => (o -> t m i) -> Producing o i m r -> t m r
replaceYield = pfold lift


-- | Take an action from i to o,
-- and create a Consuming computation that continuously
-- waits for the i, applies the action, and yields the o.
-- 
-- > foreverYield return ≡ id ∷ Consuming r m i i
foreverYield :: (Monad m) => (i -> m o) -> Consuming r m i o
foreverYield k = replaceYield (lift . k >=> yield) `overProduction` id



-- | Plug in the yield holes with a computation in the base
-- monad from o to i.
-- 
-- > yieldingTo k p ≡ p $- foreverYield k
yieldingTo :: (Monad m) => (o -> m i) -> Producing o i m r -> m r
yieldingTo = pfold id



infixr 4 /$/

-- | Composable replaceYield with monoid laws.
-- 
-- > yield /$/ x ≡ x
-- > x /$/ yield ≡ x
-- > a /$/ (b /$/ c) ≡ (a /$/ b) /$/ c
-- 
-- > return /$/ x ≡ return
-- > lift   /$/ x ≡ lift
-- > x /$/ return ≡ lift ∘ selfConnect
-- 
-- Note that when you specialize t to Producing o' i',
-- you get the type signature:
-- 
-- > (Monad m)
-- > ⇒ (a → Producing o  i  m r)
-- > → (o → Producing o' i' m i)
-- > → (a → Producing o' i' m r)
(/$/) :: (Monad m, MonadTrans t, Monad (t m))
  => (a -> Producing o i m r) -> (o -> t m i)
  -> (a -> t m r)
k1 /$/ k2 = replaceYield k2 . k1


-- Connecting computations
------------------------------------------------------------------

infixl 0 $-
-- | Connect a Producing computation with a Consuming computation
-- that has a matching interface. The two computations take turns,
-- yielding information back and forth at every switch.
-- Either one can terminate the overall computation by supplying
-- an r.
($-) :: (Monad m) => Producing a b m r -> Consuming r m a b -> m r
p $- c = resume p >>= \s -> case s of
  Done r -> return r
  Produced o k -> provide c o $- k


infixl 0 $~
-- | For when you don't want to wrap your function-to-Producing
-- in the Consuming newtype manually.
-- Using this form can look like a foreach loop:
-- 
-- example
-- 
-- > someProducer $~ \i -> do
-- >  someBodyWhichCanYieldToProducer
($~) :: (Monad m) => Producing a b m r -> (a -> Producing b a m r) -> m r
p $~ k = p $- Consuming k



-- TODO: run them in parallel with MonadPar
infixl 0 $$
-- | Connect two Producing computations together. The left one goes first,
-- and the second gets `delay`ed.
-- 
-- > p $$ p2 ≡ p $- delay p2
($$) :: (Monad m) => Producing a b m r -> Producing b a m r -> m r
p $$ p2 = p $- delay p2


-- | Connect two computations, but with the ability to resume one
-- at the point where the other terminates.
-- 
-- > connectResume (delay (return r)) k b ≡ return (Left (r, Produced b k))
connectResume :: (Monad m) => Consuming r m b a -> Consuming r' m a b -> b -> m (Resumable b a m r r')
connectResume k1 k2 = \b -> resume (provide k1 b) >>= \s -> case s of
  Done r -> return (Left (r, Produced b k2))
  Produced a k1' -> resume (provide k2 a) >>= \s2 -> case s2 of
    Done r' -> return (Right (Produced a k1', r'))
    Produced b' k2' -> connectResume k1' k2' b'


-- Misc
------------------------------------------------------------------

-- | Yield the contents of a Foldable one by one.
-- 
-- > yieldEach ≡ mapM_ yield
yieldEach :: (Monad m, Foldable f) => f a -> Producing a b m ()
yieldEach = mapM_ yield


-- | Parrot back a certain number of inputs from the interface.
-- The next value sent in gets returned as the result.
echo :: (Monad m) => Int -> Consuming a m a a
echo n | n >= 0 = Consuming $ replicateK n yield
echo _ = Consuming $ \_ -> fail "echo requires a nonnegative argument"


-- | Parrot back a certain number of inputs from the interface.
-- The next value sent in terminates the computation.
echo_ :: (Monad m) => Int -> Consuming () m a a
echo_ = voidC . echo


-- | Forget the result type. The Comsuning analogue of Control.Monad.void.
-- 
-- > voidC ≡ overProduction void
voidC :: (Monad m) => Consuming r m i o -> Consuming () m i o
voidC = overProduction void


-- As a functor in the category of monads
------------------------------------------------------------------

-- | The hoist function is fmap in the category of monads.
-- The transformation function will occur as many times
-- as the computation yields, plus one for transforming
-- the computation preceding the final result.
-- So using something like @(flip runStateT s0)@ as the argument
-- to hoist is probably not what you want, because the state would not
-- be carried from one step to the next.
-- 
-- > hoist id x ≡ x
hoist :: (Monad m, Monad n)
  => (forall x. m x -> n x)
  -> Producing o i m r -> Producing o i n r
hoist f = pfold (lift . f) yield



-- As a monad in the category of monads
-------------------------------------------------------------------

-- | The squash function is join in the category of monads.
-- It interleaves two layers of yielding on the same interface.
-- 
-- > squash (insert0 x) ≡ x
-- > squash (insert1 x) ≡ x
-- > squash (insert2 x) ≡ insert1 (squash x)
squash :: (Monad m)
  => Producing o i (Producing o i m) r
  -> Producing o i m r
squash = yieldingTo yield


-- | The embed function is bind in the category of monads.
-- It lets you convert the computations in-between yields
-- into computations over a new monad.
-- This conversion can inject new yields.
-- 
-- > embed f m ≡ squash (hoist f m)
embed :: (Monad m, Monad n)
  => (forall x. m x -> Producing i o n x)
  -> Producing i o m r -> Producing i o n r
embed f m = squash (hoist f m)


-- As an indexed comonad in the category of monads
-------------------------------------------------

-- | The selfConnection function is indexed copoint in the category of monads.
-- It allows a yielding computation to just yield to itself.
-- 
-- > selfConnection ≡ yieldingTo return
-- > selfConnection (selfConnection x) ≡ selfConnection (hoist selfConnection x)
-- 
-- Note that the following are also true:
-- 
-- > selfConnection (squash x) ≡ selfConnection (selfConnection x)
-- > selfConnection (squash x) ≡ selfConnection (hoist selfConnection x)
-- 
-- If you interleave the two layers and then performing selfConnection,
-- you might expect that sometimes one layer might yield to the other.
-- Not so. Upon yielding, when a layer switches to consuming mode,
-- it is immediately supplied with the value it just yielded.
-- Thus, each layer will always yield to itself, even though
-- the two layers' yields are interleaved.
selfConnection :: (Monad m) => Producing i i m r -> m r
selfConnection = yieldingTo return


-- | The inputThrough function is indexed extend in the category of monads.
-- .
-- Given that the monad n has the ability to simulate
-- the monad m, as well as simulate yielding on the j/k interface,
-- then you can adjust the input end of a producing computation
-- from k to j.
-- 
-- > inputThrough selfConnection x ≡ x
-- 
-- The implementation can be depicted visually like so:
-- 
-- given:
-- 
-- @
--   \/ i -> \\
-- =         p
--   \\ <- o \/
-- @
-- 
-- and given:
-- 
-- @
--   /\ x -> \
-- =        morph (yield x)
--   \\ <- i /
-- @
-- 
-- We can sort of bend these around, and combine them into:
-- 
-- @
--   \/ x -> \\       \/ i -> \\
-- -         ~morph~        |
--         \`inputThrough\`   |
-- -          - p -         |
--   \\ <- o \/       \\ <- i \/
-- @
-- 
-- From looking at the type signature,
-- this may seem like @morph@ is being used backwards.
-- Think of the morphing function as the power
-- to yield an x and get an i, so the way we use it
-- really is @x@ going in, and @i@ coming out.
inputThrough :: (Monad m, Monad n)
  => (forall z. Producing x i m z -> n z)
  -> Producing o i m r -> Producing o x n r
inputThrough morph = go where
    go p = fromStep $ morph $ liftM map' (lift (resume p))
    map' (Done r) = Done r
    map' (Produced o consuming) = Produced o $ Consuming $ \x -> do
      i <- lift (xToI x)
      go (provide consuming i)
    xToI x = morph (yield x)


-- | The through function is indexed duplicate in the category of monads.
-- 
-- The through function allows you to split one interface into two,
-- where the input of one gets fed into the output of the other.
-- 
-- > selfConnection (through x) ≡ x
-- 
-- Illustrated visually, it looks like when you go @through p@,
-- you take p's interface, and bend it around so that its
-- input and output ends are now on two separate interfaces.
-- Then you fill in the missing parts of the two interfaces
-- with a simple pass-through from one to the other.
-- 
-- @
--   \/ i -> \\
-- =         p
--   \\ <- o \/
-- @
-- 
-- @
--   \/ x -> \\       \/ x -> \\
-- -          -----         -
--           through
-- -          - p -         -
--   \\ <- o \/       \\ <- i \/
-- @
-- 
-- This "simple pass-through"
-- behavior is due to the implementation, where we pass the
-- "simple pass-through" function, @id@, to inputThrough:
-- 
-- > through ≡ inputThrough id
-- 
-- If you are wondering where the second interface hole came from,
-- given the diagram for inputThrough doesn't have it,
-- just stretch the top around to the right,
-- since @morph = id@, we have @id (yield x) = yield x@,
-- which yields over the x/i interface.
through :: (Monad m)
  => Producing o                i m  r
  -> Producing o x (Producing x i m) r
through = inputThrough id


-- Manipulating layers of Producing
------------------------------------------------------------

-- | Insert a new layer at the top of the monad transformer stack.
-- 
-- > insert0 ≡ lift
insert0 :: (Monad m, MonadTrans t, Monad (t m)) => m r -> t m r
insert0 = lift


-- | Insert a new layer one layer deep in the monad transformer stack.
-- 
-- > insert1 = hoist insert0
insert1 :: (Monad m, MonadTrans t, Monad (t m))
  => Producing o i m r
  -> Producing o i (t m) r
insert1 = hoist insert0


-- | Insert a new layer two layers deep in the monad transformer stack.
-- This pattern can be repeated ad infinitum, but you really shouldn't
-- be dealing with too many layers at the same time.
-- 
-- > insert2 = hoist insert1
insert2 :: (Monad m, MonadTrans t, Monad (t m))
  => Producing o i (Producing o' i' m) r
  -> Producing o i (Producing o' i' (t m)) r
insert2 = hoist insert1


-- | Producing layers can commute with each other.
-- 
-- > commute (commute x) ≡ x
-- > commute (lift $ yield x) ≡ yield x
-- > commute (yield x) ≡ lift $ yield x
-- > commute (return x) ≡ return x
-- > commute ∘ (f >=> g) ≡ commute ∘ f >=> commute ∘ g
commute :: (Monad m)
  => Producing a b (Producing c d m) r
  -> Producing c d (Producing a b m) r
commute p = p' $- idP where
  p' = insert2 p
  idP = insert1 `overProduction` idProxy
  idProxy = foreverYield yield
       -- = Consuming $ fix ((lift . yield >=> yield) >=>)


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
   p >>= f = fromStep $ resume p >>= \s -> case s of
     Done r -> resume (f r)
     Produced o k -> return $ Produced o $ Consuming (provide k >=> f)
   fail = lift . fail


instance MonadTrans (Producing o i) where
  lift m = fromStep $ liftM Done m


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
  listen m = fromStep $ listen (resume m) >>= \(s, w) -> case s of
    Done r -> return $ Done (r, w)
    Produced o k ->
      let k' = liftM (second (w <>)) . listen . provide k
      in return $ Produced o (Consuming k')
  -- not sure if this is legit
  pass m = fromStep $ pass $ resume m >>= \s -> case s of
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
  callCC f = fromStep $ callCC $ \k ->
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
