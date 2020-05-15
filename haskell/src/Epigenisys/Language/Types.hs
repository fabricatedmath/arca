{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Epigenisys.Language.Types where

import Control.Lens

import Control.Monad.State.Strict

import Data.Proxy (Proxy)
import Data.Text (Text)

newtype Stack a = Stack [a]
  deriving Show

type StackLens s a = Lens' s (Stack a)

class HasStackLens w a where
  stackLens :: StackLens w a

type StackFunc w = State w ()

type ProxyPartialStackOp w a = Proxy a -> PartialStackOp w

data PartialStackOp w = PartialStackOp Text (StackFunc w)

instance Show (PartialStackOp w) where
  show (PartialStackOp t _) = "PartialStackOp (" ++ show t ++ ")"

class ConvertType a b where
  convertType :: a -> b

instance ConvertType Int Float where
  convertType = fromIntegral

instance ConvertType Float Int where
  convertType = round

instance ConvertType Integer Float where
  convertType = fromIntegral

instance ConvertType Float Integer where
  convertType = round