{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Epigenisys.Language.Stack
  (
    Stack(..), StackLens, HasStackLens(..),
    HasStackLens'(..),
    popL, pushL, pushListL, empty,
    module Control.Monad.State.Strict
  ) where

import Control.Lens
import Control.Monad.State.Strict

import Data.Proxy

import GHC.Generics

import TextShow
import TextShow.Generic

newtype Stack a = 
  Stack 
  { unStack :: [a]
  }
  deriving (Show, Generic)
  deriving TextShow via FromGeneric (Stack a)

type StackLens s a = Lens' s (Stack a)

--type StackLens' s f a = StackLens s a

class HasStackLens w a where
  stackLens :: StackLens w a

class HasStackLens' w f a where
  stackLens' :: Proxy (f a) -> StackLens w a

instance forall w f a. HasStackLens w a => HasStackLens' w f a where
    stackLens' _ = stackLens :: StackLens w a

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
