{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Arca.Cuda.World.WarpFunctions where

import Arca.Cuda.World.Internal
import Arca.Cuda.World.Ops

import Arca.Language

-- maybe can add a special stack for mask and bit set?
-- to bits, from bits?

fullWarpMaskLiteral :: (HasIdentifier w, HasStackLens w U) => PartialStackOp w
fullWarpMaskLiteral = literalOp $ C_Unsigned 0xFFFFFFFF

-- Warp vote functions

{- 
int __all_sync(unsigned mask, int predicate);
int __any_sync(unsigned mask, int predicate);
unsigned __ballot_sync(unsigned mask, int predicate);
unsigned __activemask();
-}

-- Warp match functions

{- 
unsigned int __match_any_sync(unsigned mask, T value);
unsigned int __match_all_sync(unsigned mask, T value, int *pred); 
-}

-- Warp Reduce Functions (only in 8.0)

{- 
// add/min/max
unsigned __reduce_add_sync(unsigned mask, unsigned value);
unsigned __reduce_min_sync(unsigned mask, unsigned value);
unsigned __reduce_max_sync(unsigned mask, unsigned value);
int __reduce_add_sync(unsigned mask, int value);
int __reduce_min_sync(unsigned mask, int value);
int __reduce_max_sync(unsigned mask, int value);

// and/or/xor
unsigned __reduce_and_sync(unsigned mask, unsigned value);
unsigned __reduce_or_sync(unsigned mask, unsigned value);
unsigned __reduce_xor_sync(unsigned mask, unsigned value); 
-}

-- Warp Shuffle Functions

{- 
T __shfl_sync(unsigned mask, T var, int srcLane, int width=warpSize);
T __shfl_up_sync(unsigned mask, T var, unsigned int delta, int width=warpSize);
T __shfl_down_sync(unsigned mask, T var, unsigned int delta, int width=warpSize);
T __shfl_xor_sync(unsigned mask, T var, int laneMask, int width=warpSize); 
-}

-- | Direct copy from indexed lane (Automatically mods width (warpSize))
__shfl_sync :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w U, HasStackLens w o, HasStackLens w I) => Proxy a -> PartialStackOp w
__shfl_sync _ = opify (Op "__shfl_sync" :: Op (ThreeArg U o I) o)

-- | Copy from a lane with lower ID relative to caller
__shfl_up_sync :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w U, HasStackLens w o, HasStackLens w UI) => Proxy a -> PartialStackOp w
__shfl_up_sync _ = opify (Op "__shfl_up_sync" :: Op (ThreeArg U o UI) o)

-- | Copy from a lane with higher ID relative to caller
__shfl_down_sync :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w U, HasStackLens w o, HasStackLens w UI) => Proxy a -> PartialStackOp w
__shfl_down_sync _ = opify (Op "__shfl_down_sync" :: Op (ThreeArg U o UI) o)

-- | Copy from a lane based on bitwise XOR of own lane ID
__shfl_xor_sync :: forall a o w. (o ~ AST a, C_Type a, HasIdentifier w, HasStackLens w U, HasStackLens w o, HasStackLens w I) => Proxy a -> PartialStackOp w
__shfl_xor_sync _ = opify (Op "__shfl_xor_sync" :: Op (ThreeArg U o I) o)