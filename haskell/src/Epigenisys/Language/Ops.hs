{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Epigenisys.Language.Ops where

import Data.Typeable

import Epigenisys.Language.Stack
import Epigenisys.Language.Types

import TextShow

class ConvertType a b where
  convertType :: a -> b

instance ConvertType Int Float where
  convertType = fromIntegral

instance ConvertType Float Int where
  convertType = round

instance ConvertType Integer Float where
  convertType = fromIntegral

instance ConvertType Float Integer where
  convertType = round

literalOp :: forall w a. (HasStackLens w a, TextShow a) => a -> ProxyPartialStackOp w a
literalOp a _ = (PartialStackOp (showt a) $ pushL (stackLens :: StackLens w a) a)

literalOp2 :: forall w a. (HasStackLens w a, TextShow a) => Proxy a -> a -> PartialStackOp w
literalOp2 _ a = (PartialStackOp (showt a) $ pushL (stackLens :: StackLens w a) a)

convertTypeOp :: forall w a b. (HasStackLens w a, HasStackLens w b, ConvertType a b, Typeable b) => ProxyPartialStackOp w (a,b)
convertTypeOp _ = PartialStackOp opName $ convertOp (stackLens :: StackLens w a) (stackLens :: StackLens w b) convertType
  where opName = "convertTo" <> (showt $ typeRep (Proxy :: Proxy b))

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

divOp :: forall w a. (HasStackLens w a, Integral a) => ProxyPartialStackOp w a
divOp _ = PartialStackOp "div" $ binaryOp (stackLens :: StackLens w a) div

remOp :: forall w a. (HasStackLens w a, Integral a) => ProxyPartialStackOp w a
remOp _ = PartialStackOp "rem" $ binaryOp (stackLens :: StackLens w a) rem

divideOp :: forall w a. (HasStackLens w a, Fractional a) => ProxyPartialStackOp w a
divideOp _ = PartialStackOp "/" $ binaryOp (stackLens :: StackLens w a) (/)

roundOp :: forall w a b. (RealFrac a, Integral b, HasStackLens w a, HasStackLens w b) => ProxyPartialStackOp w (a,b)
roundOp _ = PartialStackOp "round" $ convertOp (stackLens :: StackLens w a) (stackLens :: StackLens w b) round

fromIntegralOp :: forall w a b. (Integral a, Num b, HasStackLens w a, HasStackLens w b) => ProxyPartialStackOp w (a,b)
fromIntegralOp _ = PartialStackOp "fromIntegral" $ convertOp (stackLens :: StackLens w a) (stackLens :: StackLens w b) fromIntegral
