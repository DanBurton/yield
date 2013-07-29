
module Control.Yield.Simple (
  P(..),
  step,
  O(..),
  ) where

import Control.Applicative
import Data.Function (fix)

data P i o r
  = Stop r
  | Step o (i -> P i o r)

step :: o -> P i o i
step o = Step o Stop

-- P i o r = Mu \s-> r + (o * s^i)

instance Functor (P i o) where
  fmap f (Stop x) = Stop (f x)
  fmap f (Step o k) = Step o (fmap f . k)

-- fmap id p = p
-- induction on p:
-- Case: p = Stop x
--   fmap id (Stop x) =?= Stop x   ==> apply fmap
--   Stop (id x)      =?= Stop x   ==> apply id
--   Stop x           === Stop x   ==> done
-- Case p = Step o k
--   fmap id (Step o k)   =?= Step o k ==> apply fmap
--   Step o (fmap id . k) =?= Step o k ==> forall K K'. K = K' -> Step o K = Step o K'
--   fmap id . k =?= k ==> apply (.) and beta expansion
--   \i -> fmap id (k i) =?= \i -> k i
--   recurse. done.


instance Applicative (P i o) where
  pure = Stop

  Stop f   <*> px = fmap f px
  Step o k <*> px = Step o ((<*> px) . k)


instance Monad (P i o) where
  return = Stop

  Stop x   >>= f = f x
  Step o k >>= f = Step o ((>>= f) . k)

(f >=> g) p = f p >>= g

-- monad laws:
-- (return >=> f) p === f p
-- (f >=> return) p === f p
-- (f >=> (g >=> h)) p === ((f >=> g) >=> h) p

-- law 1. begin with return = Stop
-- (Stop >=> f) x =?= f x ==> apply >=>
-- Stop x >>= f   =?= f x ==> apply >>=
-- f x            =?= f x ==> done

-- law 2. begin with return = Stop
-- (f >=> Stop) p =?= f p ==> apply >=>
-- f x >>= Stop   =?= f p ==> destruct (f p)

-- law 2.1, given: f p = Stop x 
-- Stop x >>= Stop =?= Stop x ==> apply >>=
-- Stop x          =?= Stop x ==> done

-- law 2.1, given: f p = Step o k
-- Step o k >>= Stop           =?= Step o k ==> apply >>=
-- Step o ((>>= Stop) . k)     =?= Step o k ==> apply (.)
-- Step o (\i -> k i >>= Stop) =?= Step o k ==> match
-- \i -> k i >>= Stop =?= \i -> k i ==> induction hypothesis

-- law 2.2
-- (f >=> (g >=> h)) p =?= ((f >=> g) >=> h) p
-- f p >>= (g >=> h)   =?= (f >=> g) p >>= h
-- f p >>= \p1 -> g p1 >>= \p2 -> h p2 =?= (f p >>= \p1 -> g p1) >>= \p2 -> h p2
-- this is intuitive from the "concatenative" nature of >>=

newtype O i o
  = O { unO :: P i o o }

-- O i o
--  = Mu \s -> o + (o * s^i)
--  = Mu \s -> o * (1 + s^i)

underO :: (O i o -> O i' o') -> P i o o -> P i' o' o'
underO f = unO . f . O

stopO :: o -> O i o
stopO o = O (Stop o)

stepO :: o -> (i -> O i o) -> O i o
stepO o k = O (Step o (unO . k))

oappend :: O i o -> (i -> O i o) -> O i o
oappend (O (Stop o)) k = stepO o k
oappend (O (Step o k)) k' = stepO o ((`oappend` k') . O . k)


instance Functor (O i) where
  fmap f (O (Stop o))   = stopO (f o)
  fmap f (O (Step o k)) = stopO (f o) `oappend` \i -> fmap f $ O (k i)

instance Applicative (O i) where
  pure = stopO

  O (Stop f)   <*> px = fmap f px
  O (Step f k) <*> px = fmap f px `oappend` \i -> O (k i) <*> px

instance Monad (O i) where
  return x = O (Stop x) -- stopO x
  O (Stop x) >>= f = f x
  O (Step x k) >>= f = f x `oappend` \i -> O (k i) >>= f
