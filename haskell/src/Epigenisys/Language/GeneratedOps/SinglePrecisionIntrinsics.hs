{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Epigenisys.Language.GeneratedOps.SinglePrecisionIntrinsics where

import Epigenisys.Sandbox.Sandbox
import Epigenisys.Language.Stack

-- | Exported Lists

f :: (HasIdentifier w, HasStackLens w F) => [State w ()]
f = [__cosf, __exp10f, __expf, __fadd_rd, __fadd_rn, __fadd_ru, __fadd_rz, __fdiv_rd, __fdiv_rn, __fdiv_ru, __fdiv_rz, __fdividef, __fmaf_rd, __fmaf_rn, __fmaf_ru, __fmaf_rz, __fmul_rd, __fmul_rn, __fmul_ru, __fmul_rz, __frcp_rd, __frcp_rn, __frcp_ru, __frcp_rz, __frsqrt_rn, __fsqrt_rd, __fsqrt_rn, __fsqrt_ru, __fsqrt_rz, __fsub_rd, __fsub_rn, __fsub_ru, __fsub_rz, __log10f, __log2f, __logf, __powf, __saturatef, __sinf, __tanf]

-- | Exported Ops

-- | Calculate the fast approximate cosine of the input argument.
__cosf :: (HasIdentifier w, HasStackLens w F) => State w ()
__cosf = opify (Op False "__cosf" :: Op (OneArg F) (OneArg F))

-- | Calculate the fast approximate base 10 exponential of the input argument.
__exp10f :: (HasIdentifier w, HasStackLens w F) => State w ()
__exp10f = opify (Op False "__exp10f" :: Op (OneArg F) (OneArg F))

-- | Calculate the fast approximate base e exponential of the input argument.
__expf :: (HasIdentifier w, HasStackLens w F) => State w ()
__expf = opify (Op False "__expf" :: Op (OneArg F) (OneArg F))

-- | Add two floating point values in round-down mode.
__fadd_rd :: (HasIdentifier w, HasStackLens w F) => State w ()
__fadd_rd = opify (Op False "__fadd_rd" :: Op (TwoArg F F) (OneArg F))

-- | Add two floating point values in round-to-nearest-even mode.
__fadd_rn :: (HasIdentifier w, HasStackLens w F) => State w ()
__fadd_rn = opify (Op False "__fadd_rn" :: Op (TwoArg F F) (OneArg F))

-- | Add two floating point values in round-up mode.
__fadd_ru :: (HasIdentifier w, HasStackLens w F) => State w ()
__fadd_ru = opify (Op False "__fadd_ru" :: Op (TwoArg F F) (OneArg F))

-- | Add two floating point values in round-towards-zero mode.
__fadd_rz :: (HasIdentifier w, HasStackLens w F) => State w ()
__fadd_rz = opify (Op False "__fadd_rz" :: Op (TwoArg F F) (OneArg F))

-- | Divide two floating point values in round-down mode.
__fdiv_rd :: (HasIdentifier w, HasStackLens w F) => State w ()
__fdiv_rd = opify (Op False "__fdiv_rd" :: Op (TwoArg F F) (OneArg F))

-- | Divide two floating point values in round-to-nearest-even mode.
__fdiv_rn :: (HasIdentifier w, HasStackLens w F) => State w ()
__fdiv_rn = opify (Op False "__fdiv_rn" :: Op (TwoArg F F) (OneArg F))

-- | Divide two floating point values in round-up mode.
__fdiv_ru :: (HasIdentifier w, HasStackLens w F) => State w ()
__fdiv_ru = opify (Op False "__fdiv_ru" :: Op (TwoArg F F) (OneArg F))

-- | Divide two floating point values in round-towards-zero mode.
__fdiv_rz :: (HasIdentifier w, HasStackLens w F) => State w ()
__fdiv_rz = opify (Op False "__fdiv_rz" :: Op (TwoArg F F) (OneArg F))

-- | Calculate the fast approximate division of the input arguments.
__fdividef :: (HasIdentifier w, HasStackLens w F) => State w ()
__fdividef = opify (Op False "__fdividef" :: Op (TwoArg F F) (OneArg F))

-- | Compute x × y + z as a single operation, in round-down mode.
__fmaf_rd :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmaf_rd = opify (Op False "__fmaf_rd" :: Op (ThreeArg F F F) (OneArg F))

-- | Compute x × y + z as a single operation, in round-to-nearest-even mode.
__fmaf_rn :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmaf_rn = opify (Op False "__fmaf_rn" :: Op (ThreeArg F F F) (OneArg F))

-- | Compute x × y + z as a single operation, in round-up mode.
__fmaf_ru :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmaf_ru = opify (Op False "__fmaf_ru" :: Op (ThreeArg F F F) (OneArg F))

-- | Compute x × y + z as a single operation, in round-towards-zero mode.
__fmaf_rz :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmaf_rz = opify (Op False "__fmaf_rz" :: Op (ThreeArg F F F) (OneArg F))

-- | Multiply two floating point values in round-down mode.
__fmul_rd :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmul_rd = opify (Op False "__fmul_rd" :: Op (TwoArg F F) (OneArg F))

-- | Multiply two floating point values in round-to-nearest-even mode.
__fmul_rn :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmul_rn = opify (Op False "__fmul_rn" :: Op (TwoArg F F) (OneArg F))

-- | Multiply two floating point values in round-up mode.
__fmul_ru :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmul_ru = opify (Op False "__fmul_ru" :: Op (TwoArg F F) (OneArg F))

-- | Multiply two floating point values in round-towards-zero mode.
__fmul_rz :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmul_rz = opify (Op False "__fmul_rz" :: Op (TwoArg F F) (OneArg F))

-- | Compute 1 x in round-down mode.
__frcp_rd :: (HasIdentifier w, HasStackLens w F) => State w ()
__frcp_rd = opify (Op False "__frcp_rd" :: Op (OneArg F) (OneArg F))

-- | Compute 1 x in round-to-nearest-even mode.
__frcp_rn :: (HasIdentifier w, HasStackLens w F) => State w ()
__frcp_rn = opify (Op False "__frcp_rn" :: Op (OneArg F) (OneArg F))

-- | Compute 1 x in round-up mode.
__frcp_ru :: (HasIdentifier w, HasStackLens w F) => State w ()
__frcp_ru = opify (Op False "__frcp_ru" :: Op (OneArg F) (OneArg F))

-- | Compute 1 x in round-towards-zero mode.
__frcp_rz :: (HasIdentifier w, HasStackLens w F) => State w ()
__frcp_rz = opify (Op False "__frcp_rz" :: Op (OneArg F) (OneArg F))

-- | Compute 1 / x in round-to-nearest-even mode.
__frsqrt_rn :: (HasIdentifier w, HasStackLens w F) => State w ()
__frsqrt_rn = opify (Op False "__frsqrt_rn" :: Op (OneArg F) (OneArg F))

-- | Compute x in round-down mode.
__fsqrt_rd :: (HasIdentifier w, HasStackLens w F) => State w ()
__fsqrt_rd = opify (Op False "__fsqrt_rd" :: Op (OneArg F) (OneArg F))

-- | Compute x in round-to-nearest-even mode.
__fsqrt_rn :: (HasIdentifier w, HasStackLens w F) => State w ()
__fsqrt_rn = opify (Op False "__fsqrt_rn" :: Op (OneArg F) (OneArg F))

-- | Compute x in round-up mode.
__fsqrt_ru :: (HasIdentifier w, HasStackLens w F) => State w ()
__fsqrt_ru = opify (Op False "__fsqrt_ru" :: Op (OneArg F) (OneArg F))

-- | Compute x in round-towards-zero mode.
__fsqrt_rz :: (HasIdentifier w, HasStackLens w F) => State w ()
__fsqrt_rz = opify (Op False "__fsqrt_rz" :: Op (OneArg F) (OneArg F))

-- | Subtract two floating point values in round-down mode.
__fsub_rd :: (HasIdentifier w, HasStackLens w F) => State w ()
__fsub_rd = opify (Op False "__fsub_rd" :: Op (TwoArg F F) (OneArg F))

-- | Subtract two floating point values in round-to-nearest-even mode.
__fsub_rn :: (HasIdentifier w, HasStackLens w F) => State w ()
__fsub_rn = opify (Op False "__fsub_rn" :: Op (TwoArg F F) (OneArg F))

-- | Subtract two floating point values in round-up mode.
__fsub_ru :: (HasIdentifier w, HasStackLens w F) => State w ()
__fsub_ru = opify (Op False "__fsub_ru" :: Op (TwoArg F F) (OneArg F))

-- | Subtract two floating point values in round-towards-zero mode.
__fsub_rz :: (HasIdentifier w, HasStackLens w F) => State w ()
__fsub_rz = opify (Op False "__fsub_rz" :: Op (TwoArg F F) (OneArg F))

-- | Calculate the fast approximate base 10 logarithm of the input argument.
__log10f :: (HasIdentifier w, HasStackLens w F) => State w ()
__log10f = opify (Op False "__log10f" :: Op (OneArg F) (OneArg F))

-- | Calculate the fast approximate base 2 logarithm of the input argument.
__log2f :: (HasIdentifier w, HasStackLens w F) => State w ()
__log2f = opify (Op False "__log2f" :: Op (OneArg F) (OneArg F))

-- | Calculate the fast approximate base e logarithm of the input argument.
__logf :: (HasIdentifier w, HasStackLens w F) => State w ()
__logf = opify (Op False "__logf" :: Op (OneArg F) (OneArg F))

-- | Calculate the fast approximate of x y .
__powf :: (HasIdentifier w, HasStackLens w F) => State w ()
__powf = opify (Op False "__powf" :: Op (TwoArg F F) (OneArg F))

-- | Clamp the input argument to [+0.0, 1.0].
__saturatef :: (HasIdentifier w, HasStackLens w F) => State w ()
__saturatef = opify (Op False "__saturatef" :: Op (OneArg F) (OneArg F))

-- | Failed to parse (Has Void Type):
-- |
-- | 	__device__   void __sincosf ( float  x, float* sptr, float* cptr )
-- | 	    Calculate the fast approximate of sine and cosine of the first input argument. 

-- | Calculate the fast approximate sine of the input argument.
__sinf :: (HasIdentifier w, HasStackLens w F) => State w ()
__sinf = opify (Op False "__sinf" :: Op (OneArg F) (OneArg F))

-- | Calculate the fast approximate tangent of the input argument.
__tanf :: (HasIdentifier w, HasStackLens w F) => State w ()
__tanf = opify (Op False "__tanf" :: Op (OneArg F) (OneArg F))

