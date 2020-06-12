{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Epigenisys.Worlds.CudaWorld.Ops where

import Data.Proxy

import TextShow

import Epigenisys.Language.Stack
import Epigenisys.Language.Types
import Epigenisys.Worlds.CudaWorld.Internal.AST

-- | Helper to build stack op that drops 'a' value into an 'AST a' literal
literalOp :: forall a w. (TextShow a, HasIdentifier w, HasStackLens w (AST a)) => a -> PartialStackOp w
literalOp a = PartialStackOp (showt a) $ 
    do
        i <- ratchetId
        pushL (stackLens :: StackLens w (AST a)) $ Literal i a

addOp :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
addOp _ = opify (Op True "+" :: Op (TwoArg o o) (OneArg o))

subtractOp :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
subtractOp _ = opify (Op True "-" :: Op (TwoArg o o) (OneArg o))

multiplyOp :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
multiplyOp _ = opify (Op True "*" :: Op (TwoArg o o) (OneArg o))

divideOp :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
divideOp _ = opify (Op True "/" :: Op (TwoArg o o) (OneArg o))

dupOp :: forall a w. HasStackLens w a => Proxy a -> PartialStackOp w
dupOp _ = PartialStackOp "dup" $  do
    me <- popL (stackLens :: StackLens w a)
    case me of
        Nothing -> return ()
        Just e -> do
            pushListL stackLens [e,e]