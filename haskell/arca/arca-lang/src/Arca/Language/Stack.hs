{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Arca.Language.Stack
  ( Stack(..), StackLens, HasStackLens(..)
  , popL, pushL, pushListL, empty
  , module Control.Monad.State.Strict
  , popLT, StackFunc, StackFunc'
  ) where

import Control.Lens
import Control.Monad.State.Strict

import GHC.Generics

import TextShow
import TextShow.Generic

newtype Stack a = 
  Stack { unStack :: [a] }
  deriving (Show, Generic)
  deriving TextShow via FromGeneric (Stack a)

type StackLens s a = Lens' s (Stack a)

class HasStackLens w a where
  stackLens :: StackLens w a

type StackFunc' w a = StateT w Maybe a
type StackFunc w = StackFunc' w ()

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

mpop :: Stack a -> Maybe (a, Stack a)
mpop (Stack (a:as)) = Just (a, Stack as)
mpop (Stack []) = Nothing
{-# INLINEABLE mpop #-}

popL :: MonadState s m => StackLens s a -> m (Maybe a)
popL l = l %%= runState pop
{-# INLINEABLE popL #-}

popLT :: StackLens s a -> StackFunc' s a
popLT l = zoom l $ StateT mpop
{-# INLINEABLE popLT #-}

pushL :: MonadState s m => StackLens s a -> a -> m ()
pushL l c = l %= push' c
{-# INLINEABLE pushL #-}

pushListL :: MonadState s m => StackLens s a -> [a] -> m ()
pushListL l cs = l %= pushList' cs
{-# INLINEABLE pushListL #-}
