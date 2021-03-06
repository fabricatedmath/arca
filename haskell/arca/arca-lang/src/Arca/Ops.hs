{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Arca.Ops where

import Arca.Language

import TextShow

literalOp :: forall w a. (HasStackLens w a, TextShow a) => Proxy a -> a -> PartialStackOp w
literalOp _ a = (PartialStackOp (OpName $ showt a) $ pushL (stackLens :: StackLens w a) a)

convertOp :: forall w a b. (HasStackLens w a, HasStackLens w b) => OpName -> (a -> b) -> PartialStackOp w
convertOp opname f = PartialStackOp opname $ convertOp' (stackLens :: StackLens w a) (stackLens :: StackLens w b) f

convertOp' :: StackLens w a -> StackLens w b -> (a -> b) -> StackFunc w
convertOp' la lb f = 
  do
    a <- popLT la
    pushL lb $ f a

binaryOp :: StackLens w a -> (a -> a -> a) -> StackFunc w
binaryOp l f =
  do
    a <- popLT l
    b <- popLT l
    pushL l $ f a b

unaryOp :: StackLens w a -> (a -> a) -> StackFunc w
unaryOp l f =
  do
    a <- popLT l
    pushL l $ f a 

addOp :: forall w a. (HasStackLens w a, Num a) => Proxy a -> PartialStackOp w
addOp _ = PartialStackOp (OpName "+") $ binaryOp (stackLens :: StackLens w a) (+)

subtractOp :: forall w a. (HasStackLens w a, Num a) => Proxy a -> PartialStackOp w
subtractOp _ = PartialStackOp (OpName "-") $ binaryOp (stackLens :: StackLens w a) (-)

multiplyOp :: forall w a. (HasStackLens w a, Num a) => Proxy a -> PartialStackOp w
multiplyOp _ = PartialStackOp (OpName "*") $ binaryOp (stackLens :: StackLens w a) (*)

divOp :: forall w a. (HasStackLens w a, Integral a) => Proxy a -> PartialStackOp w
divOp _ = PartialStackOp (OpName "div") $ binaryOp (stackLens :: StackLens w a) div

remOp :: forall w a. (HasStackLens w a, Integral a) => Proxy a -> PartialStackOp w
remOp _ = PartialStackOp (OpName "rem") $ binaryOp (stackLens :: StackLens w a) rem

divideOp :: forall w a. (HasStackLens w a, Fractional a) => Proxy a -> PartialStackOp w
divideOp _ = PartialStackOp (OpName "/") $ binaryOp (stackLens :: StackLens w a) (/)

roundOp :: forall w a b. (RealFrac a, Integral b, HasStackLens w a, HasStackLens w b) => Proxy (a,b) -> PartialStackOp w
roundOp _ = PartialStackOp (OpName "round") $ convertOp' (stackLens :: StackLens w a) (stackLens :: StackLens w b) round

fromIntegralOp :: forall w a b. (Integral a, Num b, HasStackLens w a, HasStackLens w b) => Proxy (a,b) -> PartialStackOp w
fromIntegralOp _ = PartialStackOp (OpName "fromIntegral") $ convertOp' (stackLens :: StackLens w a) (stackLens :: StackLens w b) fromIntegral
