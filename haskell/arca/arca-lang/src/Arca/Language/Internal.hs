{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Arca.Language.Internal where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (Reader)
import qualified Data.Text.Lazy.Builder as T

import GHC.Generics

import TextShow
import TextShow.Generic

import Arca.Language.Stack

type LiteralParser w = Text -> Either String (PartialStackOp w)
data NamespaceOps w = NamespaceOps Namespace (Maybe (LiteralParser w)) [PartialStackOp w]

class HasNamespaces w where
  getNamespaces :: [NamespaceOps w]

data PartialStackOp w = PartialStackOp OpName (StackFunc w) 

instance Semigroup (PartialStackOp w) where
  (<>) (PartialStackOp o1 sf1) (PartialStackOp o2 sf2) = PartialStackOp (o1 <> o2) (sf1 *> sf2)

instance Monoid (PartialStackOp w) where
  mempty = PartialStackOp mempty (pure ())

newtype Namespace = Namespace { unNamespace :: Text }
  deriving (Eq, Generic, Ord, Show)
  deriving TextShow via FromGeneric Namespace

newtype OpName = OpName { unOpName :: Text }
  deriving (Eq, Generic, Ord, Show)
  deriving TextShow via FromGeneric OpName

instance Semigroup OpName where
  (<>) (OpName n1) (OpName n2) = OpName $ n1 <> " - " <> n2

instance Monoid OpName where
  mempty = OpName mempty

data StackOpText = StackOpText Namespace OpName
  deriving (Generic, Show)
  deriving TextShow via FromGeneric StackOpText

data StackOp w = 
  StackOp 
  { stackOpFunc :: StackFunc w
  , stackOpName :: OpName
  , stackOpNamespace :: Namespace
  }

-- | Collapses a StateT s Maybe () into a State s ()
-- | Maybe forces a rolledback state if transaction fails
transformStackFunc :: MonadState w m => StackFunc w -> m ()
transformStackFunc mt = 
  do
    w <- get
    maybe (put w) put $ execStateT mt w

psoToSo :: Namespace -> PartialStackOp w -> StackOp w
psoToSo namespace (PartialStackOp opName f) = 
  StackOp 
  { stackOpFunc = f
  , stackOpName = opName
  , stackOpNamespace = namespace
  }

soToPso :: StackOp w -> PartialStackOp w
soToPso sop = PartialStackOp (stackOpName sop) (stackOpFunc sop)

instance Eq (StackOp w) where
  (==) sop1 sop2 = 
    stackOpNamespace sop1 == stackOpNamespace sop2 && 
    stackOpName sop1 == stackOpName sop2

instance Show (StackOp w) where
  show = T.unpack . showt

instance TextShow (StackOp w) where
  showb sop = T.fromText (unNamespace $ stackOpNamespace sop) <> singleton '.' <> T.fromText (unOpName $ stackOpName sop)

textRead :: Reader a -> Text -> Either String a
textRead reader t = 
  either (\a -> Left $ "On input " ++ show t ++ ": " ++ a) Right $ do
    (i,t') <- reader t
    if T.null t' 
      then return i
      else Left $ "Failed to consume all input on literal: " ++ show t

-- | Takes an f like :: a -> b -> (,) a b and pops the appropriate stacks to load this type up.
class CurryState w f a where
  curryState :: f -> StateT w Maybe a

instance CurryState w a a where
  curryState a = pure a

instance 
  ( CurryState w b c, HasStackLens w a
  ) => CurryState w (a -> b) c where
  curryState f = popLT stackLens >>= curryState . f

