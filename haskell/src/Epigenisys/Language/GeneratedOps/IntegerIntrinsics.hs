{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Epigenisys.Language.GeneratedOps.IntegerIntrinsics where

import Epigenisys.Sandbox.Sandbox
import Epigenisys.Language.Stack

-- | Exported Lists

i :: (HasIdentifier w, HasStackLens w I) => [State w ()]
i = [__clz, __ffs, __hadd, __mul24, __mulhi, __rhadd]

i_ui :: (HasIdentifier w, HasStackLens w I, HasStackLens w UI) => [State w ()]
i_ui = [__popc, __sad]

i_l :: (HasIdentifier w, HasStackLens w I, HasStackLens w L) => [State w ()]
i_l = [__clzll, __ffsll]

i_ul :: (HasIdentifier w, HasStackLens w I, HasStackLens w UL) => [State w ()]
i_ul = [__popcll]

ui :: (HasIdentifier w, HasStackLens w UI) => [State w ()]
ui = [__brev, __byte_perm, __funnelshift_l, __funnelshift_lc, __funnelshift_r, __funnelshift_rc, __uhadd, __umul24, __umulhi, __urhadd, __usad]

l :: (HasIdentifier w, HasStackLens w L) => [State w ()]
l = [__mul64hi]

ul :: (HasIdentifier w, HasStackLens w UL) => [State w ()]
ul = [__brevll, __umul64hi]

-- | Exported Ops

-- | Reverse the bit order of a 32 bit unsigned integer.
__brev :: (HasIdentifier w, HasStackLens w UI) => State w ()
__brev = opify (Op False "__brev" :: Op (OneArg UI) (OneArg UI))

-- | Reverse the bit order of a 64 bit unsigned integer.
__brevll :: (HasIdentifier w, HasStackLens w UL) => State w ()
__brevll = opify (Op False "__brevll" :: Op (OneArg UL) (OneArg UL))

-- | Return selected bytes from two 32 bit unsigned integers.
__byte_perm :: (HasIdentifier w, HasStackLens w UI) => State w ()
__byte_perm = opify (Op False "__byte_perm" :: Op (ThreeArg UI UI UI) (OneArg UI))

-- | Return the number of consecutive high-order zero bits in a 32 bit integer.
__clz :: (HasIdentifier w, HasStackLens w I) => State w ()
__clz = opify (Op False "__clz" :: Op (OneArg I) (OneArg I))

-- | Count the number of consecutive high-order zero bits in a 64 bit integer.
__clzll :: (HasIdentifier w, HasStackLens w I, HasStackLens w L) => State w ()
__clzll = opify (Op False "__clzll" :: Op (OneArg L) (OneArg I))

-- | Find the position of the least significant bit set to 1 in a 32 bit integer.
__ffs :: (HasIdentifier w, HasStackLens w I) => State w ()
__ffs = opify (Op False "__ffs" :: Op (OneArg I) (OneArg I))

-- | Find the position of the least significant bit set to 1 in a 64 bit integer.
__ffsll :: (HasIdentifier w, HasStackLens w I, HasStackLens w L) => State w ()
__ffsll = opify (Op False "__ffsll" :: Op (OneArg L) (OneArg I))

-- | Concatenate hi : lo, shift left by shift & 31 bits, return the most significant 32 bits.
__funnelshift_l :: (HasIdentifier w, HasStackLens w UI) => State w ()
__funnelshift_l = opify (Op False "__funnelshift_l" :: Op (ThreeArg UI UI UI) (OneArg UI))

-- | Concatenate hi : lo, shift left by min(shift, 32) bits, return the most significant 32 bits.
__funnelshift_lc :: (HasIdentifier w, HasStackLens w UI) => State w ()
__funnelshift_lc = opify (Op False "__funnelshift_lc" :: Op (ThreeArg UI UI UI) (OneArg UI))

-- | Concatenate hi : lo, shift right by shift & 31 bits, return the least significant 32 bits.
__funnelshift_r :: (HasIdentifier w, HasStackLens w UI) => State w ()
__funnelshift_r = opify (Op False "__funnelshift_r" :: Op (ThreeArg UI UI UI) (OneArg UI))

-- | Concatenate hi : lo, shift right by min(shift, 32) bits, return the least significant 32 bits.
__funnelshift_rc :: (HasIdentifier w, HasStackLens w UI) => State w ()
__funnelshift_rc = opify (Op False "__funnelshift_rc" :: Op (ThreeArg UI UI UI) (OneArg UI))

-- | Compute average of signed input arguments, avoiding overflow in the intermediate sum.
__hadd :: (HasIdentifier w, HasStackLens w I) => State w ()
__hadd = opify (Op False "__hadd" :: Op (TwoArg I I) (OneArg I))

-- | Calculate the least significant 32 bits of the product of the least significant 24 bits of two integers.
__mul24 :: (HasIdentifier w, HasStackLens w I) => State w ()
__mul24 = opify (Op False "__mul24" :: Op (TwoArg I I) (OneArg I))

-- | Calculate the most significant 64 bits of the product of the two 64 bit integers.
__mul64hi :: (HasIdentifier w, HasStackLens w L) => State w ()
__mul64hi = opify (Op False "__mul64hi" :: Op (TwoArg L L) (OneArg L))

-- | Calculate the most significant 32 bits of the product of the two 32 bit integers.
__mulhi :: (HasIdentifier w, HasStackLens w I) => State w ()
__mulhi = opify (Op False "__mulhi" :: Op (TwoArg I I) (OneArg I))

-- | Count the number of bits that are set to 1 in a 32 bit integer.
__popc :: (HasIdentifier w, HasStackLens w I, HasStackLens w UI) => State w ()
__popc = opify (Op False "__popc" :: Op (OneArg UI) (OneArg I))

-- | Count the number of bits that are set to 1 in a 64 bit integer.
__popcll :: (HasIdentifier w, HasStackLens w I, HasStackLens w UL) => State w ()
__popcll = opify (Op False "__popcll" :: Op (OneArg UL) (OneArg I))

-- | Compute rounded average of signed input arguments, avoiding overflow in the intermediate sum.
__rhadd :: (HasIdentifier w, HasStackLens w I) => State w ()
__rhadd = opify (Op False "__rhadd" :: Op (TwoArg I I) (OneArg I))

-- | Calculate | x − y | + z , the sum of absolute difference.
__sad :: (HasIdentifier w, HasStackLens w I, HasStackLens w UI) => State w ()
__sad = opify (Op False "__sad" :: Op (ThreeArg I I UI) (OneArg UI))

-- | Compute average of unsigned input arguments, avoiding overflow in the intermediate sum.
__uhadd :: (HasIdentifier w, HasStackLens w UI) => State w ()
__uhadd = opify (Op False "__uhadd" :: Op (TwoArg UI UI) (OneArg UI))

-- | Calculate the least significant 32 bits of the product of the least significant 24 bits of two unsigned integers.
__umul24 :: (HasIdentifier w, HasStackLens w UI) => State w ()
__umul24 = opify (Op False "__umul24" :: Op (TwoArg UI UI) (OneArg UI))

-- | Calculate the most significant 64 bits of the product of the two 64 unsigned bit integers.
__umul64hi :: (HasIdentifier w, HasStackLens w UL) => State w ()
__umul64hi = opify (Op False "__umul64hi" :: Op (TwoArg UL UL) (OneArg UL))

-- | Calculate the most significant 32 bits of the product of the two 32 bit unsigned integers.
__umulhi :: (HasIdentifier w, HasStackLens w UI) => State w ()
__umulhi = opify (Op False "__umulhi" :: Op (TwoArg UI UI) (OneArg UI))

-- | Compute rounded average of unsigned input arguments, avoiding overflow in the intermediate sum.
__urhadd :: (HasIdentifier w, HasStackLens w UI) => State w ()
__urhadd = opify (Op False "__urhadd" :: Op (TwoArg UI UI) (OneArg UI))

-- | Calculate | x − y | + z , the sum of absolute difference.
__usad :: (HasIdentifier w, HasStackLens w UI) => State w ()
__usad = opify (Op False "__usad" :: Op (ThreeArg UI UI UI) (OneArg UI))

