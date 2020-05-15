{-# LANGUAGE RankNTypes #-}

module Epigenisys.Language.Stack
  (
    Stack, StackLens,
    popL, pushL, pushListL,
    empty,
    module Control.Monad.State.Strict
  ) where

import Control.Lens
import Control.Monad.State.Strict

import Epigenisys.Language.Types

empty :: Stack a
empty = Stack []
{-# INLINEABLE empty #-}

pop :: State (Stack a) (Maybe a)
pop = state pop'
{-# INLINEABLE pop #-}

push' :: a -> Stack a -> Stack a
push' a (Stack as) = Stack (a:as)
{-# INLINEABLE push' #-}

pushList' :: [a] -> Stack a -> Stack a
pushList' as (Stack as') = Stack (as ++ as')
{-# INLINEABLE pushList' #-}

pop' :: Stack a -> (Maybe a, Stack a)
pop' (Stack (a:as)) = (Just a, Stack as)
pop' (Stack []) = (Nothing, Stack [])
{-# INLINEABLE pop' #-}

popL :: MonadState s m => StackLens s a -> m (Maybe a)
popL l = l %%= runState pop
{-# INLINEABLE popL #-}

pushL :: MonadState s m => StackLens s a -> a -> m ()
pushL l c = l %= push' c
{-# INLINEABLE pushL #-}

pushListL :: MonadState s m => StackLens s a -> [a] -> m ()
pushListL l cs = l %= pushList' cs
{-# INLINEABLE pushListL #-}
