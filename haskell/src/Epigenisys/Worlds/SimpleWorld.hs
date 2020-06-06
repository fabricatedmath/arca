{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Epigenisys.Worlds.SimpleWorld where

import Control.Lens

import Data.Proxy
import qualified Data.Text.Read as T (decimal, signed, rational)

import GHC.Generics

import TextShow
import TextShow.Generic

import Epigenisys.Language
import Epigenisys.Language.Parser
import Epigenisys.Language.Ops 
import Epigenisys.Language.Stack

data World =
  World
  { _execStack :: Stack (Exec World)
  , _intStack :: Stack Int
  , _integerStack :: Stack Integer
  , _floatStack :: Stack Float
  } deriving (Generic, Show)
    deriving TextShow via FromGeneric World

makeLenses ''World

floatNamespaceOps :: NamespaceOps World
floatNamespaceOps = NamespaceOps (Namespace "Float") literalParser floatOps
  where
    floatProxy = Proxy :: Proxy Float
    floatIntProxy = Proxy :: Proxy (Float,Int)
    floatIntegerProxy = Proxy :: Proxy (Float,Integer)
    floatOps = 
      [ addOp floatProxy
      , subtractOp floatProxy
      , multiplyOp floatProxy
      , divideOp floatProxy
      , convertOp floatIntProxy "round" round
      , convertOp floatIntegerProxy "fromIntegral" round
      ]
    literalParser = Just $ fmap (literalOp floatProxy) . textRead (T.signed T.rational)

intNamespaceOps :: NamespaceOps World
intNamespaceOps = NamespaceOps (Namespace "Int") literalParser intOps
  where
    intProxy = Proxy :: Proxy Int
    intFloatProxy = Proxy :: Proxy (Int,Float)
    intOps = 
      [ addOp intProxy
      , subtractOp intProxy
      , multiplyOp intProxy
      , divOp intProxy
      , remOp intProxy
      , convertOp intFloatProxy "fromIntegral" fromIntegral
      ]
    literalParser = Just $ fmap (literalOp intProxy) . textRead (T.signed T.decimal)

integerNamespaceOps :: NamespaceOps World
integerNamespaceOps = NamespaceOps (Namespace "Integer") literalParser intOps
  where
    intProxy = Proxy :: Proxy Integer
    intFloatProxy = Proxy :: Proxy (Integer,Float)
    intOps = 
      [ addOp intProxy
      , subtractOp intProxy
      , multiplyOp intProxy
      , divOp intProxy
      , remOp intProxy
      , convertOp intFloatProxy "fromIntegral" fromIntegral
      ]
    literalParser = Just $ fmap (literalOp intProxy) . textRead (T.signed T.decimal)

instance HasNamespaces World where
  getNamespaces = 
    [ intNamespaceOps
    , integerNamespaceOps
    , floatNamespaceOps
    ]
 
instance HasEmpty World where
  getEmpty = 
    World 
    { _execStack = empty
    , _intStack = empty
    , _integerStack = empty
    , _floatStack = empty
    }

instance HasStackLens World (Exec World) where
  stackLens = execStack

instance HasStackLens World Integer where
  stackLens = integerStack

instance HasStackLens World Int where
  stackLens = intStack

instance HasStackLens World Float where
  stackLens = floatStack

