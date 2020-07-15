{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Arca.Cuda.World.Ops where

import Data.Proxy

import Data.Text (Text)
import TextShow

import Arca.Language
import Arca.Cuda.World.Internal.AST
import Arca.Cuda.World.Internal.Operator

-- | Helper to build stack op that drops 'a' value into an 'AST a' literal
literalOp :: forall a w. (TextShow a, HasIdentifier w, HasStackLens w (AST a)) => a -> PartialStackOp w
literalOp a = PartialStackOp (OpName $ showt a) $ 
    do
        i <- ratchetId
        pushL (stackLens :: StackLens w (AST a)) $ Literal i a

callOp :: forall a w. (TextShow a, HasIdentifier w, HasStackLens w (AST a)) => Proxy a -> OpName -> Text -> PartialStackOp w
callOp _ callName call = PartialStackOp callName $ 
    do
        i <- ratchetId
        pushL (stackLens :: StackLens w (AST a)) $ Call i call

addOp :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
addOp _ = opify (Op "+" :: Op (TwoArgInfix o o) o)

subtractOp :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
subtractOp _ = opify (Op "-" :: Op (TwoArgInfix o o) o)

multiplyOp :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
multiplyOp _ = opify (Op "*" :: Op (TwoArgInfix o o) o)

divideOp :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
divideOp _ = opify (Op "/" :: Op (TwoArgInfix o o) o)

dupOp :: forall a w. HasStackLens w a => Proxy a -> PartialStackOp w
dupOp _ = PartialStackOp (OpName "dup") $  do
    me <- popL (stackLens :: StackLens w a)
    case me of
        Nothing -> return ()
        Just e -> pushListL stackLens [e,e]

dupOpAST :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w o) => Proxy a -> PartialStackOp w
dupOpAST _ = PartialStackOp (OpName "dup") $  do
    me <- popL (stackLens :: StackLens w o)
    case me of
        Nothing -> return ()
        Just e -> pushListL stackLens [e,e]