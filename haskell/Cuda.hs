module Cuda where

import Something

-- | Exported Lists

f :: (HasStackLens w F) => [State w ()]
f = [hypotf, acosf, acoshf, asinf, asinhf, atan2f, atanf, atanhf, cbrtf, ceilf]

flul :: (HasStackLens w F, HasStackLens w L, HasStackLens w UL) => [State w ()]
flul = [acosf]

ui :: (HasStackLens w UI) => [State w ()]
ui = [__vabsdiffs2]

-- | Exported Ops

-- | Calculate the square root of the sum of squares of two arguments.
hypotf :: (HasStackLens w F) => State w ()
hypotf = opify (Op False "hypotf" :: Op (OneArg F) (TwoArg F F))

-- | Computes per-halfword sum of absolute difference of signed integer.
__vabsdiffs2 :: (HasStackLens w UI) => State w ()
__vabsdiffs2 = opify (Op False "__vabsdiffs2" :: Op (OneArg UI) (TwoArg UI UI))

-- | sfafdsf
acosf :: (HasStackLens w F, HasStackLens w L, HasStackLens w UL) => State w ()
acosf = opify (Op False "acosf" :: Op (OneArg L) (ThreeArg F F UL))

-- | Calculate the arc cosine of the input argument.
acosf :: (HasStackLens w F) => State w ()
acosf = opify (Op False "acosf" :: Op (OneArg F) (OneArg F))

-- | Calculate the nonnegative arc hyperbolic cosine of the input argument.
acoshf :: (HasStackLens w F) => State w ()
acoshf = opify (Op False "acoshf" :: Op (OneArg F) (OneArg F))

-- | Calculate the arc sine of the input argument.
asinf :: (HasStackLens w F) => State w ()
asinf = opify (Op False "asinf" :: Op (OneArg F) (OneArg F))

-- | Calculate the arc hyperbolic sine of the input argument.
asinhf :: (HasStackLens w F) => State w ()
asinhf = opify (Op False "asinhf" :: Op (OneArg F) (OneArg F))

-- | Calculate the arc tangent of the ratio of first and second input arguments.
atan2f :: (HasStackLens w F) => State w ()
atan2f = opify (Op False "atan2f" :: Op (OneArg F) (TwoArg F F))

-- | Calculate the arc tangent of the input argument.
atanf :: (HasStackLens w F) => State w ()
atanf = opify (Op False "atanf" :: Op (OneArg F) (OneArg F))

-- | Calculate the arc hyperbolic tangent of the input argument.
atanhf :: (HasStackLens w F) => State w ()
atanhf = opify (Op False "atanhf" :: Op (OneArg F) (OneArg F))

-- | Calculate the cube root of the input argument.
cbrtf :: (HasStackLens w F) => State w ()
cbrtf = opify (Op False "cbrtf" :: Op (OneArg F) (OneArg F))

-- | Calculate ceiling of the input argument.
ceilf :: (HasStackLens w F) => State w ()
ceilf = opify (Op False "ceilf" :: Op (OneArg F) (OneArg F))

-- | Failed to parse (Has Void Type):
-- |
-- | 	__device__   void __sincosf ( float  x, float* sptr, float* cptr )
-- | 	    Calculate the fast approximate of sine and cosine of the first input argument. 

-- | Failed to parse (Has Void Type):
-- |
-- | 	__device__   void __sinf ( float  x )
-- | 	    Calculate the fast approximate sine of the input argument. 

-- | Failed to parse (Has Pointer Type):
-- |
-- | 	__device__   float __tanf ( float*  x )
-- | 	    Calculate the fast approximate tangent of the input argument. 

