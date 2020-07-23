{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.SpecialOps where
{-
shfl :: Proxy a -> PartialStackOp w
shfl _ = PartialStackOp (OpName "shfl") $ do
    me <- popL (stackLens :: StackLens w a)
-}

import Data.Proxy

import Arca.Cuda.World.Internal
import Arca.Cuda.World.Ops

import Arca.Language

testOp :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
testOp = mconcat
    [ literalOp $ C_Float 2
    , literalOp $ C_Float 1
    , addOp (Proxy :: Proxy C_Float)
    ]

shfl_sync_op :: (C_Type a, HasIdentifier w, HasStackLens w (AST a), HasStackLens w F, HasStackLens w I, HasStackLens w U) => Proxy a -> PartialStackOp w
shfl_sync_op proxy = mconcat
    [ literalOp $ C_Unsigned 0x1F
    , literalOp $ C_Int 0x1F
    , bitAndOp (Proxy :: Proxy C_Int)
    , __shfl_sync proxy
    ]



