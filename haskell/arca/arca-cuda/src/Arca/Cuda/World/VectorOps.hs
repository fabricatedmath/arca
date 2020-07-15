{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.VectorOps where

import Arca.Cuda.World.Internal

import Arca.Language

unpackF2 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F2) => PartialStackOp w
unpackF2 = accify (Acc "unpack_float2" twoField :: Acc F2 (TwoField F F))
    where twoField = TwoField "x" "y"

packF2 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F2) => PartialStackOp w
packF2 = opify (Op "make_float2" :: Op (TwoArg F F) (OneArg F2))

unpackF4 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F4) => PartialStackOp w
unpackF4 = accify (Acc "unpack_float4" fourField :: Acc F4 (FourField F F F F))
    where fourField = FourField "x" "y" "z" "w"

packF4 :: (HasIdentifier w, HasStackLens w F, HasStackLens w F4) => PartialStackOp w
packF4 = opify (Op "make_float4" :: Op (FourArg F F F F) (OneArg F4))