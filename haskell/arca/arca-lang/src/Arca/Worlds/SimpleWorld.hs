{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Arca.Worlds.SimpleWorld where

import Control.Lens

import qualified Data.Text.Read as T (decimal, signed, rational)

import GHC.Generics

import TextShow
import TextShow.Generic

import Arca.Language
import Arca.Ops 

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
    floatOps = 
      [ addOp floatProxy
      , subtractOp floatProxy
      , multiplyOp floatProxy
      , divideOp floatProxy
      , convertOp (OpName "roundToInt") (round :: Float -> Int)
      , convertOp  (OpName "roundToInteger") (round :: Float -> Int)
      ]
    literalParser = Just $ fmap (literalOp floatProxy) . textRead (T.signed T.rational)

intNamespaceOps :: NamespaceOps World
intNamespaceOps = NamespaceOps (Namespace "Int") literalParser intOps
  where
    intProxy = Proxy :: Proxy Int
    intOps = 
      [ addOp intProxy
      , subtractOp intProxy
      , multiplyOp intProxy
      , divOp intProxy
      , remOp intProxy
      , convertOp (OpName "fromIntegral") (fromIntegral :: Int -> Float)
      ]
    literalParser = Just $ fmap (literalOp intProxy) . textRead (T.signed T.decimal)

integerNamespaceOps :: NamespaceOps World
integerNamespaceOps = NamespaceOps (Namespace "Integer") literalParser intOps
  where
    intProxy = Proxy :: Proxy Integer
    intOps = 
      [ addOp intProxy
      , subtractOp intProxy
      , multiplyOp intProxy
      , divOp intProxy
      , remOp intProxy
      , convertOp (OpName "fromIntegral") (fromIntegral :: Integer -> Float)
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

