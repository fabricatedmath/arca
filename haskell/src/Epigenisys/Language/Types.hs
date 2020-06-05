{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Epigenisys.Language.Types where

import Control.Monad.State.Strict (State)

import Data.Proxy (Proxy)
import Data.Text (Text)

import qualified Data.Text as T

import GHC.Generics

import TextShow
import TextShow.Generic

type StackFunc w = State w ()

type ProxyPartialStackOp w a = Proxy a -> PartialStackOp w

data PartialStackOp w = PartialStackOp Text (StackFunc w)
{-
instance Show (PartialStackOp w) where
  show (PartialStackOp t _) = "PartialStackOp (" ++ show t ++ ")"

instance TextShow (PartialStackOp w) where
  showb (PartialStackOp t _) = "PartialStackOp (" <> fromText t <> ")"
  -}

newtype Namespace = Namespace { unNamespace :: Text }
  deriving (Eq, Generic, Ord, Show)
  deriving TextShow via FromGeneric Namespace

newtype OpName = OpName { unOpName :: Text }
  deriving (Eq, Generic, Ord, Show)
  deriving TextShow via FromGeneric OpName

data StackOpText = StackOpText Namespace OpName
  deriving (Generic, Show)
  deriving TextShow via FromGeneric StackOpText

data StackOp w = 
  StackOp 
  { stackOpFunc :: StackFunc w
  , stackOpName :: OpName
  , stackOpNamespace :: Namespace
  }

instance Show (StackOp w) where
  show sop = 
    T.unpack $ 
    unNamespace (stackOpNamespace sop) <> "." <> unOpName (stackOpName sop)

instance TextShow (StackOp w) where
  showb sop = 
    (fromText $ unNamespace $ stackOpNamespace sop) <> singleton '.' <> (fromText $ unOpName $ stackOpName sop)