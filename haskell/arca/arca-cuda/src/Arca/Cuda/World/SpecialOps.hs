{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.SpecialOps where
{-
shfl :: Proxy a -> PartialStackOp w
shfl _ = PartialStackOp (OpName "shfl") $ do
    me <- popL (stackLens :: StackLens w a)
-}

import Arca.Cuda.World.Internal
import Arca.Cuda.World.Ops
import Arca.Cuda.World.WarpFunctions

import Arca.Language

testOp :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
testOp = mconcat
    [ literalOp $ C_Float 2
    , literalOp $ C_Float 1
    , addOp (Proxy :: Proxy C_Float)
    ]

shfl_sync_op :: 
    ( C_Type a, HasIdentifier w
    , HasStackLens w (AST a)
    , HasStackLens w F
    , HasStackLens w I
    , HasStackLens w U
    ) => Proxy a -> PartialStackOp w
shfl_sync_op proxy = mconcat
    [ fullWarpMaskLiteral
    , __shfl_sync proxy
    ]


