{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.VectorOps where

import Arca.Cuda.World.Internal

import Arca.Language

unpackF2 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F2) => PartialStackOp w
unpackF2 = accify (Acc "unpack_float2" twoField :: Acc F2 (TwoField F F))
    where twoField = TwoField "x" "y"

packF2 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F2) => PartialStackOp w
packF2 = opify (Op "make_float2" :: Op (TwoArg F F) F2)

unpackF4 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F4) => PartialStackOp w
unpackF4 = accify (Acc "unpack_float4" fourField :: Acc F4 (FourField F F F F))
    where fourField = FourField "x" "y" "z" "w"

packF4 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F4) => PartialStackOp w
packF4 = opify (Op "make_float4" :: Op (FourArg F F F F) F4)

unpackI2 :: (HasIdentifier w, HasStackLens w I, HasStackLens w I2) => PartialStackOp w
unpackI2 = accify (Acc "unpack_int2" twoField :: Acc I2 (TwoField I I))
    where twoField = TwoField "x" "y"

packI2 :: (HasIdentifier w, HasStackLens w I, HasStackLens w I2) => PartialStackOp w
packI2 = opify (Op "make_int2" :: Op (TwoArg I I) I2)

unpackI4 :: (HasIdentifier w, HasStackLens w I, HasStackLens w I4) => PartialStackOp w
unpackI4 = accify (Acc "unpack_int4" fourField :: Acc I4 (FourField I I I I))
    where fourField = FourField "x" "y" "z" "w"

packI4 :: (HasIdentifier w, HasStackLens w I, HasStackLens w I4) => PartialStackOp w
packI4 = opify (Op "make_int4" :: Op (FourArg I I I I) I4)
