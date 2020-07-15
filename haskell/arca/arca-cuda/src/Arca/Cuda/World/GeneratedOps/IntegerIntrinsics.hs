{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.GeneratedOps.IntegerIntrinsics where

import Arca.Language
import Arca.Cuda.World.Internal

-- | Exported Lists

i :: (HasIdentifier w, HasStackLens w I) => [PartialStackOp w]
i = [__clz, __ffs, __hadd, __mul24, __mulhi, __rhadd]

i_ui :: (HasIdentifier w, HasStackLens w I, HasStackLens w UI) => [PartialStackOp w]
i_ui = [__popc, __sad]

i_l :: (HasIdentifier w, HasStackLens w I, HasStackLens w L) => [PartialStackOp w]
i_l = [__clzll, __ffsll]

i_ul :: (HasIdentifier w, HasStackLens w I, HasStackLens w UL) => [PartialStackOp w]
i_ul = [__popcll]

ui :: (HasIdentifier w, HasStackLens w UI) => [PartialStackOp w]
ui = [__brev, __byte_perm, __funnelshift_l, __funnelshift_lc, __funnelshift_r, __funnelshift_rc, __uhadd, __umul24, __umulhi, __urhadd, __usad]

l :: (HasIdentifier w, HasStackLens w L) => [PartialStackOp w]
l = [__mul64hi]

ul :: (HasIdentifier w, HasStackLens w UL) => [PartialStackOp w]
ul = [__brevll, __umul64hi]

-- | Exported Ops

-- | Reverse the bit order of a 32 bit unsigned integer.
__brev :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__brev = opify (Op "__brev" :: Op (OneArg UI) UI)

-- | Reverse the bit order of a 64 bit unsigned integer.
__brevll :: (HasIdentifier w, HasStackLens w UL) => PartialStackOp w
__brevll = opify (Op "__brevll" :: Op (OneArg UL) UL)

-- | Return selected bytes from two 32 bit unsigned integers.
__byte_perm :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__byte_perm = opify (Op "__byte_perm" :: Op (ThreeArg UI UI UI) UI)

-- | Return the number of consecutive high-order zero bits in a 32 bit integer.
__clz :: (HasIdentifier w, HasStackLens w I) => PartialStackOp w
__clz = opify (Op "__clz" :: Op (OneArg I) I)

-- | Count the number of consecutive high-order zero bits in a 64 bit integer.
__clzll :: (HasIdentifier w, HasStackLens w I, HasStackLens w L) => PartialStackOp w
__clzll = opify (Op "__clzll" :: Op (OneArg L) I)

-- | Find the position of the least significant bit set to 1 in a 32 bit integer.
__ffs :: (HasIdentifier w, HasStackLens w I) => PartialStackOp w
__ffs = opify (Op "__ffs" :: Op (OneArg I) I)

-- | Find the position of the least significant bit set to 1 in a 64 bit integer.
__ffsll :: (HasIdentifier w, HasStackLens w I, HasStackLens w L) => PartialStackOp w
__ffsll = opify (Op "__ffsll" :: Op (OneArg L) I)

-- | Concatenate hi : lo, shift left by shift & 31 bits, return the most significant 32 bits.
__funnelshift_l :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__funnelshift_l = opify (Op "__funnelshift_l" :: Op (ThreeArg UI UI UI) UI)

-- | Concatenate hi : lo, shift left by min(shift, 32) bits, return the most significant 32 bits.
__funnelshift_lc :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__funnelshift_lc = opify (Op "__funnelshift_lc" :: Op (ThreeArg UI UI UI) UI)

-- | Concatenate hi : lo, shift right by shift & 31 bits, return the least significant 32 bits.
__funnelshift_r :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__funnelshift_r = opify (Op "__funnelshift_r" :: Op (ThreeArg UI UI UI) UI)

-- | Concatenate hi : lo, shift right by min(shift, 32) bits, return the least significant 32 bits.
__funnelshift_rc :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__funnelshift_rc = opify (Op "__funnelshift_rc" :: Op (ThreeArg UI UI UI) UI)

-- | Compute average of signed input arguments, avoiding overflow in the intermediate sum.
__hadd :: (HasIdentifier w, HasStackLens w I) => PartialStackOp w
__hadd = opify (Op "__hadd" :: Op (TwoArg I I) I)

-- | Calculate the least significant 32 bits of the product of the least significant 24 bits of two integers.
__mul24 :: (HasIdentifier w, HasStackLens w I) => PartialStackOp w
__mul24 = opify (Op "__mul24" :: Op (TwoArg I I) I)

-- | Calculate the most significant 64 bits of the product of the two 64 bit integers.
__mul64hi :: (HasIdentifier w, HasStackLens w L) => PartialStackOp w
__mul64hi = opify (Op "__mul64hi" :: Op (TwoArg L L) L)

-- | Calculate the most significant 32 bits of the product of the two 32 bit integers.
__mulhi :: (HasIdentifier w, HasStackLens w I) => PartialStackOp w
__mulhi = opify (Op "__mulhi" :: Op (TwoArg I I) I)

-- | Count the number of bits that are set to 1 in a 32 bit integer.
__popc :: (HasIdentifier w, HasStackLens w I, HasStackLens w UI) => PartialStackOp w
__popc = opify (Op "__popc" :: Op (OneArg UI) I)

-- | Count the number of bits that are set to 1 in a 64 bit integer.
__popcll :: (HasIdentifier w, HasStackLens w I, HasStackLens w UL) => PartialStackOp w
__popcll = opify (Op "__popcll" :: Op (OneArg UL) I)

-- | Compute rounded average of signed input arguments, avoiding overflow in the intermediate sum.
__rhadd :: (HasIdentifier w, HasStackLens w I) => PartialStackOp w
__rhadd = opify (Op "__rhadd" :: Op (TwoArg I I) I)

-- | Calculate | x − y | + z , the sum of absolute difference.
__sad :: (HasIdentifier w, HasStackLens w I, HasStackLens w UI) => PartialStackOp w
__sad = opify (Op "__sad" :: Op (ThreeArg I I UI) UI)

-- | Compute average of unsigned input arguments, avoiding overflow in the intermediate sum.
__uhadd :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__uhadd = opify (Op "__uhadd" :: Op (TwoArg UI UI) UI)

-- | Calculate the least significant 32 bits of the product of the least significant 24 bits of two unsigned integers.
__umul24 :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__umul24 = opify (Op "__umul24" :: Op (TwoArg UI UI) UI)

-- | Calculate the most significant 64 bits of the product of the two 64 unsigned bit integers.
__umul64hi :: (HasIdentifier w, HasStackLens w UL) => PartialStackOp w
__umul64hi = opify (Op "__umul64hi" :: Op (TwoArg UL UL) UL)

-- | Calculate the most significant 32 bits of the product of the two 32 bit unsigned integers.
__umulhi :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__umulhi = opify (Op "__umulhi" :: Op (TwoArg UI UI) UI)

-- | Compute rounded average of unsigned input arguments, avoiding overflow in the intermediate sum.
__urhadd :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__urhadd = opify (Op "__urhadd" :: Op (TwoArg UI UI) UI)

-- | Calculate | x − y | + z , the sum of absolute difference.
__usad :: (HasIdentifier w, HasStackLens w UI) => PartialStackOp w
__usad = opify (Op "__usad" :: Op (ThreeArg UI UI UI) UI)

