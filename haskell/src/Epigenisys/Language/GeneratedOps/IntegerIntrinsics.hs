module Epigenisys.Language.GeneratedOps.IntegerIntrinsics where

import Something

-- | Exported Lists

i :: (HasStackLens w I) => [State w ()]
i = [__clz, __ffs, __hadd, __mul24, __mulhi, __rhadd]

i_ui :: (HasStackLens w I, HasStackLens w UI) => [State w ()]
i_ui = [__popc, __sad]

i_l :: (HasStackLens w I, HasStackLens w L) => [State w ()]
i_l = [__clzll, __ffsll]

i_ul :: (HasStackLens w I, HasStackLens w UL) => [State w ()]
i_ul = [__popcll]

ui :: (HasStackLens w UI) => [State w ()]
ui = [__brev, __byte_perm, __funnelshift_l, __funnelshift_lc, __funnelshift_r, __funnelshift_rc, __uhadd, __umul24, __umulhi, __urhadd, __usad]

l :: (HasStackLens w L) => [State w ()]
l = [__mul64hi]

ul :: (HasStackLens w UL) => [State w ()]
ul = [__brevll, __umul64hi]

-- | Exported Ops

-- | Reverse the bit order of a 32 bit unsigned integer.
__brev :: (HasStackLens w UI) => State w ()
__brev = opify (Op False "__brev" :: Op (OneArg UI) (OneArg UI))

-- | Reverse the bit order of a 64 bit unsigned integer.
__brevll :: (HasStackLens w UL) => State w ()
__brevll = opify (Op False "__brevll" :: Op (OneArg UL) (OneArg UL))

-- | Return selected bytes from two 32 bit unsigned integers.
__byte_perm :: (HasStackLens w UI) => State w ()
__byte_perm = opify (Op False "__byte_perm" :: Op (OneArg UI) (ThreeArg UI UI UI))

-- | Return the number of consecutive high-order zero bits in a 32 bit integer.
__clz :: (HasStackLens w I) => State w ()
__clz = opify (Op False "__clz" :: Op (OneArg I) (OneArg I))

-- | Count the number of consecutive high-order zero bits in a 64 bit integer.
__clzll :: (HasStackLens w I, HasStackLens w L) => State w ()
__clzll = opify (Op False "__clzll" :: Op (OneArg I) (OneArg L))

-- | Find the position of the least significant bit set to 1 in a 32 bit integer.
__ffs :: (HasStackLens w I) => State w ()
__ffs = opify (Op False "__ffs" :: Op (OneArg I) (OneArg I))

-- | Find the position of the least significant bit set to 1 in a 64 bit integer.
__ffsll :: (HasStackLens w I, HasStackLens w L) => State w ()
__ffsll = opify (Op False "__ffsll" :: Op (OneArg I) (OneArg L))

-- | Concatenate hi : lo, shift left by shift & 31 bits, return the most significant 32 bits.
__funnelshift_l :: (HasStackLens w UI) => State w ()
__funnelshift_l = opify (Op False "__funnelshift_l" :: Op (OneArg UI) (ThreeArg UI UI UI))

-- | Concatenate hi : lo, shift left by min(shift, 32) bits, return the most significant 32 bits.
__funnelshift_lc :: (HasStackLens w UI) => State w ()
__funnelshift_lc = opify (Op False "__funnelshift_lc" :: Op (OneArg UI) (ThreeArg UI UI UI))

-- | Concatenate hi : lo, shift right by shift & 31 bits, return the least significant 32 bits.
__funnelshift_r :: (HasStackLens w UI) => State w ()
__funnelshift_r = opify (Op False "__funnelshift_r" :: Op (OneArg UI) (ThreeArg UI UI UI))

-- | Concatenate hi : lo, shift right by min(shift, 32) bits, return the least significant 32 bits.
__funnelshift_rc :: (HasStackLens w UI) => State w ()
__funnelshift_rc = opify (Op False "__funnelshift_rc" :: Op (OneArg UI) (ThreeArg UI UI UI))

-- | Compute average of signed input arguments, avoiding overflow in the intermediate sum.
__hadd :: (HasStackLens w I) => State w ()
__hadd = opify (Op False "__hadd" :: Op (OneArg I) (TwoArg I I))

-- | Calculate the least significant 32 bits of the product of the least significant 24 bits of two integers.
__mul24 :: (HasStackLens w I) => State w ()
__mul24 = opify (Op False "__mul24" :: Op (OneArg I) (TwoArg I I))

-- | Calculate the most significant 64 bits of the product of the two 64 bit integers.
__mul64hi :: (HasStackLens w L) => State w ()
__mul64hi = opify (Op False "__mul64hi" :: Op (OneArg L) (TwoArg L L))

-- | Calculate the most significant 32 bits of the product of the two 32 bit integers.
__mulhi :: (HasStackLens w I) => State w ()
__mulhi = opify (Op False "__mulhi" :: Op (OneArg I) (TwoArg I I))

-- | Count the number of bits that are set to 1 in a 32 bit integer.
__popc :: (HasStackLens w I, HasStackLens w UI) => State w ()
__popc = opify (Op False "__popc" :: Op (OneArg I) (OneArg UI))

-- | Count the number of bits that are set to 1 in a 64 bit integer.
__popcll :: (HasStackLens w I, HasStackLens w UL) => State w ()
__popcll = opify (Op False "__popcll" :: Op (OneArg I) (OneArg UL))

-- | Compute rounded average of signed input arguments, avoiding overflow in the intermediate sum.
__rhadd :: (HasStackLens w I) => State w ()
__rhadd = opify (Op False "__rhadd" :: Op (OneArg I) (TwoArg I I))

-- | Calculate | x − y | + z , the sum of absolute difference.
__sad :: (HasStackLens w I, HasStackLens w UI) => State w ()
__sad = opify (Op False "__sad" :: Op (OneArg UI) (ThreeArg I I UI))

-- | Compute average of unsigned input arguments, avoiding overflow in the intermediate sum.
__uhadd :: (HasStackLens w UI) => State w ()
__uhadd = opify (Op False "__uhadd" :: Op (OneArg UI) (TwoArg UI UI))

-- | Calculate the least significant 32 bits of the product of the least significant 24 bits of two unsigned integers.
__umul24 :: (HasStackLens w UI) => State w ()
__umul24 = opify (Op False "__umul24" :: Op (OneArg UI) (TwoArg UI UI))

-- | Calculate the most significant 64 bits of the product of the two 64 unsigned bit integers.
__umul64hi :: (HasStackLens w UL) => State w ()
__umul64hi = opify (Op False "__umul64hi" :: Op (OneArg UL) (TwoArg UL UL))

-- | Calculate the most significant 32 bits of the product of the two 32 bit unsigned integers.
__umulhi :: (HasStackLens w UI) => State w ()
__umulhi = opify (Op False "__umulhi" :: Op (OneArg UI) (TwoArg UI UI))

-- | Compute rounded average of unsigned input arguments, avoiding overflow in the intermediate sum.
__urhadd :: (HasStackLens w UI) => State w ()
__urhadd = opify (Op False "__urhadd" :: Op (OneArg UI) (TwoArg UI UI))

-- | Calculate | x − y | + z , the sum of absolute difference.
__usad :: (HasStackLens w UI) => State w ()
__usad = opify (Op False "__usad" :: Op (OneArg UI) (ThreeArg UI UI UI))

