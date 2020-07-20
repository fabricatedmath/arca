{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.VectorOps where

import Arca.Cuda.World.Internal

import Arca.Language

unpackF2 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F2) => PartialStackOp w
unpackF2 = accify (Acc "unpack_float2" vfield2 :: Acc F2 (VField2 F))

packF2 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F2) => PartialStackOp w
packF2 = opify (Op "make_float2" :: Op (VArg2 F) F2)

unpackF4 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F4) => PartialStackOp w
unpackF4 = accify (Acc "unpack_float4" vfield4 :: Acc F4 (VField4 F))

packF4 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F4) => PartialStackOp w
packF4 = opify (Op "make_float4" :: Op (VArg4 F) F4)

unpackI2 :: (HasIdentifier w, HasStackLens w I, HasStackLens w I2) => PartialStackOp w
unpackI2 = accify (Acc "unpack_int2" vfield2 :: Acc I2 (VField2 I))

packI2 :: (HasIdentifier w, HasStackLens w I, HasStackLens w I2) => PartialStackOp w
packI2 = opify (Op "make_int2" :: Op (VArg2 I) I2)

unpackI4 :: (HasIdentifier w, HasStackLens w I, HasStackLens w I4) => PartialStackOp w
unpackI4 = accify (Acc "unpack_int4" vfield4 :: Acc I4 (VField4 I))

packI4 :: (HasIdentifier w, HasStackLens w I, HasStackLens w I4) => PartialStackOp w
packI4 = opify (Op "make_int4" :: Op (VArg4 I) I4)
