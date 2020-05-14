{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Epigenisys.Ops where

import qualified Data.Text as T (append, pack)
import Data.Typeable

import Epigenisys.Stack (get, put, popL, pushL)
import Epigenisys.Types

literalOp :: forall w a. (HasStackLens w a, Show a) => a -> ProxyPartialStackOp w a
literalOp a _ = (PartialStackOp (T.pack $ show a) $ pushL (stackLens :: StackLens w a) a)

convertTypeOp :: forall w a b. (HasStackLens w a, HasStackLens w b, ConvertType a b, Typeable b) => ProxyPartialStackOp w (a,b)
convertTypeOp _ = PartialStackOp opName $ convertOp (stackLens :: StackLens w a) (stackLens :: StackLens w b) convertType
  where opName = "convertTo" `T.append` (T.pack $ show $ typeRep (Proxy :: Proxy b))

convertOp :: StackLens w a -> StackLens w b -> (a -> b) -> StackFunc w
convertOp la lb f = 
  do
    w <- get
    a <- popL la
    maybe (put w) (pushL lb) $ f <$> a

binaryOp :: StackLens w a -> (a -> a -> a) -> StackFunc w
binaryOp l f =
  do
    w <- get
    a <- popL l
    b <- popL l
    maybe (put w) (pushL l) $ f <$> a <*> b

unaryOp :: StackLens w a -> (a -> a) -> StackFunc w
unaryOp l f =
  do
    w <- get
    a <- popL l
    maybe (put w) (pushL l) $ f <$> a


addOp :: forall w a. (HasStackLens w a, Num a) => ProxyPartialStackOp w a
addOp _ = PartialStackOp "+" $ binaryOp (stackLens :: StackLens w a) (+)

subtractOp :: forall w a. (HasStackLens w a, Num a) => ProxyPartialStackOp w a
subtractOp _ = PartialStackOp "-" $ binaryOp (stackLens :: StackLens w a) (-)

multiplyOp :: forall w a. (HasStackLens w a, Num a) => ProxyPartialStackOp w a
multiplyOp _ = PartialStackOp "*" $ binaryOp (stackLens :: StackLens w a) (*)

divideOp :: forall w a. (HasStackLens w a, Fractional a) => ProxyPartialStackOp w a
divideOp _ = PartialStackOp "/" $ binaryOp (stackLens :: StackLens w a) (/)

divOp :: forall w a. (HasStackLens w a, Integral a) => ProxyPartialStackOp w a
divOp _ = PartialStackOp "div" $ binaryOp (stackLens :: StackLens w a) div

remOp :: forall w a. (HasStackLens w a, Integral a) => ProxyPartialStackOp w a
remOp _ = PartialStackOp "rem" $ binaryOp (stackLens :: StackLens w a) rem


