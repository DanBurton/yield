{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}

module Util where

import Control.Monad

type family Fst (xy :: (*,*)) :: *
type family Snd (xy :: (*,*)) :: *
type family Flip (xy :: (*,*)) :: (*,*)

type instance Fst '(x,y) = x
type instance Snd '(x,y) = y
type instance Flip '(x,y) = '(y,x)


foreverK :: Monad m => (a -> m a) -> a -> m r
foreverK f = let go = f >=> go in go

replicateK :: Monad m => Int -> (a -> m a) -> a -> m a
replicateK n k = iterate (k >=>) return !! n

