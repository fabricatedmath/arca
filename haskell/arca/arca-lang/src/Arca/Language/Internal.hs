{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Arca.Language.Internal where

import Data.Proxy (Proxy)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (Reader)
import qualified Data.Text.Lazy.Builder as T

import GHC.Generics

import TextShow
import TextShow.Generic

import Arca.Language.Stack (State)

type LiteralParser w = Text -> Either String (PartialStackOp w)
data NamespaceOps w = NamespaceOps Namespace (Maybe (LiteralParser w)) [PartialStackOp w]

class HasNamespaces w where
  getNamespaces :: [NamespaceOps w]

type StackFunc w = State w ()

type ProxyPartialStackOp w a = Proxy a -> PartialStackOp w

data PartialStackOp w = PartialStackOp OpName (StackFunc w)

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
  show = T.unpack . showt

instance TextShow (StackOp w) where
  showb sop = T.fromText (unNamespace $ stackOpNamespace sop) <> singleton '.' <> T.fromText (unOpName $ stackOpName sop)

applyNamespace :: Namespace -> PartialStackOp w -> StackOp w
applyNamespace namespace (PartialStackOp opName f) = 
  StackOp 
  { stackOpFunc = f
  , stackOpName = opName
  , stackOpNamespace = namespace
  }

textRead :: Reader a -> Text -> Either String a
textRead reader t = 
  either (\a -> Left $ "On input " ++ show t ++ ": " ++ a) Right $ do
    (i,t') <- reader t
    if T.null t' 
      then return i
      else Left $ "Failed to consume all input on literal: " ++ show t