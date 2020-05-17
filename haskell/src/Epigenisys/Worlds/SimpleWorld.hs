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
  (HasStack(..), HasWorldParser(..), StackName(..), StackType(..), textRead)
import Epigenisys.Language.Ops 
import Epigenisys.Language.Stack
import Epigenisys.Language.Types (HasStackLens(..))

data World =
  World
  { _execStack :: Stack (Exec World)
  , _intStack :: Stack Int
  , _integerStack :: Stack Integer
  , _floatStack :: Stack Float
  } deriving (Generic, Show)
    deriving TextShow via FromGeneric World

makeLenses ''World

instance HasEmpty World where
  getEmpty = 
    World 
    { _execStack = empty
    , _intStack = empty
    , _integerStack = empty
    , _floatStack = empty
    }

instance HasWorldParser World where
  worldTypes = 
    [ StackType (Proxy :: Proxy (World, Exec World))
    , StackType (Proxy :: Proxy (World, Int))
    , StackType (Proxy :: Proxy (World, Integer))
    , StackType (Proxy :: Proxy (World, Float))
    ]

instance HasStackLens World (Exec World) where
  stackLens = execStack

instance HasStack World (Exec World) where
  stackName _ = StackName "Exec"
  stackOps _ = []
  stackParseLiteral _ = Nothing

instance HasStackLens World Integer where
  stackLens = integerStack

instance HasStack World Integer where
  stackName _ = StackName "Integer"
  stackOps _ = 
    [ addOp integerProxy
    , subtractOp integerProxy
    , multiplyOp integerProxy
    , divOp integerProxy
    , remOp integerProxy
    , convertTypeOp integerFloatProxy 
    ] where integerProxy = Proxy :: Proxy Integer
            integerFloatProxy = Proxy :: Proxy (Integer,Float)
  stackParseLiteral _ = Just $ textRead $ T.signed T.decimal

instance HasStackLens World Int where
  stackLens = intStack

instance HasStack World Int where
  stackName _ = StackName "Int"
  stackOps _ = 
    [ addOp intProxy
    , subtractOp intProxy
    , multiplyOp intProxy
    , divOp intProxy
    , remOp intProxy
    , convertTypeOp intFloatProxy
    ] where intProxy = Proxy :: Proxy Int
            intFloatProxy = Proxy :: Proxy (Int,Float)
  stackParseLiteral _ = Just $ textRead $ T.signed T.decimal

instance HasStackLens World Float where
  stackLens = floatStack

instance HasStack World Float where
  stackName _ = StackName "Float"
  stackOps _ = 
    [ addOp floatProxy
    , subtractOp floatProxy
    , multiplyOp floatProxy
    , divideOp floatProxy
    , convertTypeOp floatIntProxy
    , convertTypeOp floatIntegerProxy
    ] where 
        floatProxy = Proxy :: Proxy Float
        floatIntProxy = Proxy :: Proxy (Float,Int)
        floatIntegerProxy = Proxy :: Proxy (Float,Integer)
  stackParseLiteral _ = Just $ textRead $ T.signed T.rational
