{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Epigenisys.Worlds.CudaWorld.GeneratedOps.SinglePrecisionMathematicalFunctions where

import Epigenisys.Language
import Epigenisys.Worlds.CudaWorld.Internal

-- | Exported Lists

f :: (HasIdentifier w, HasStackLens w F) => [PartialStackOp w]
f = [acosf, acoshf, asinf, asinhf, atan2f, atanf, atanhf, cbrtf, ceilf, copysignf, cosf, coshf, cospif, cyl_bessel_i0f, cyl_bessel_i1f, erfcf, erfcinvf, erfcxf, erff, erfinvf, exp10f, exp2f, expf, expm1f, fabsf, fdimf, fdividef, floorf, fmaf, fmaxf, fminf, fmodf, hypotf, j0f, j1f, lgammaf, log10f, log1pf, log2f, logbf, logf, nearbyintf, nextafterf, norm3df, norm4df, normcdff, normcdfinvf, powf, rcbrtf, remainderf, rhypotf, rintf, rnorm3df, rnorm4df, roundf, rsqrtf, sinf, sinhf, sinpif, sqrtf, tanf, tanhf, tgammaf, truncf, y0f, y1f]

f_i :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => [PartialStackOp w]
f_i = [ilogbf, isfinite, isinf, isnan, jnf, ldexpf, scalbnf, signbit, ynf]

f_li :: (HasIdentifier w, HasStackLens w F, HasStackLens w LI) => [PartialStackOp w]
f_li = [lrintf, lroundf, scalblnf]

f_l :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => [PartialStackOp w]
f_l = [llrintf, llroundf]

-- | Exported Ops

-- | Calculate the arc cosine of the input argument.
acosf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
acosf = opify (Op False "acosf" :: Op (OneArg F) (OneArg F))

-- | Calculate the nonnegative arc hyperbolic cosine of the input argument.
acoshf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
acoshf = opify (Op False "acoshf" :: Op (OneArg F) (OneArg F))

-- | Calculate the arc sine of the input argument.
asinf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
asinf = opify (Op False "asinf" :: Op (OneArg F) (OneArg F))

-- | Calculate the arc hyperbolic sine of the input argument.
asinhf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
asinhf = opify (Op False "asinhf" :: Op (OneArg F) (OneArg F))

-- | Calculate the arc tangent of the ratio of first and second input arguments.
atan2f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
atan2f = opify (Op False "atan2f" :: Op (TwoArg F F) (OneArg F))

-- | Calculate the arc tangent of the input argument.
atanf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
atanf = opify (Op False "atanf" :: Op (OneArg F) (OneArg F))

-- | Calculate the arc hyperbolic tangent of the input argument.
atanhf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
atanhf = opify (Op False "atanhf" :: Op (OneArg F) (OneArg F))

-- | Calculate the cube root of the input argument.
cbrtf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cbrtf = opify (Op False "cbrtf" :: Op (OneArg F) (OneArg F))

-- | Calculate ceiling of the input argument.
ceilf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
ceilf = opify (Op False "ceilf" :: Op (OneArg F) (OneArg F))

-- | Create value with given magnitude, copying sign of second value.
copysignf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
copysignf = opify (Op False "copysignf" :: Op (TwoArg F F) (OneArg F))

-- | Calculate the cosine of the input argument.
cosf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cosf = opify (Op False "cosf" :: Op (OneArg F) (OneArg F))

-- | Calculate the hyperbolic cosine of the input argument.
coshf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
coshf = opify (Op False "coshf" :: Op (OneArg F) (OneArg F))

-- | Calculate the cosine of the input argument × π .
cospif :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cospif = opify (Op False "cospif" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the regular modified cylindrical Bessel function of order 0 for the input argument.
cyl_bessel_i0f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cyl_bessel_i0f = opify (Op False "cyl_bessel_i0f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the regular modified cylindrical Bessel function of order 1 for the input argument.
cyl_bessel_i1f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cyl_bessel_i1f = opify (Op False "cyl_bessel_i1f" :: Op (OneArg F) (OneArg F))

-- | Calculate the complementary error function of the input argument.
erfcf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erfcf = opify (Op False "erfcf" :: Op (OneArg F) (OneArg F))

-- | Calculate the inverse complementary error function of the input argument.
erfcinvf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erfcinvf = opify (Op False "erfcinvf" :: Op (OneArg F) (OneArg F))

-- | Calculate the scaled complementary error function of the input argument.
erfcxf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erfcxf = opify (Op False "erfcxf" :: Op (OneArg F) (OneArg F))

-- | Calculate the error function of the input argument.
erff :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erff = opify (Op False "erff" :: Op (OneArg F) (OneArg F))

-- | Calculate the inverse error function of the input argument.
erfinvf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erfinvf = opify (Op False "erfinvf" :: Op (OneArg F) (OneArg F))

-- | Calculate the base 10 exponential of the input argument.
exp10f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
exp10f = opify (Op False "exp10f" :: Op (OneArg F) (OneArg F))

-- | Calculate the base 2 exponential of the input argument.
exp2f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
exp2f = opify (Op False "exp2f" :: Op (OneArg F) (OneArg F))

-- | Calculate the base e exponential of the input argument.
expf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
expf = opify (Op False "expf" :: Op (OneArg F) (OneArg F))

-- | Calculate the base e exponential of the input argument, minus 1.
expm1f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
expm1f = opify (Op False "expm1f" :: Op (OneArg F) (OneArg F))

-- | Calculate the absolute value of its argument.
fabsf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fabsf = opify (Op False "fabsf" :: Op (OneArg F) (OneArg F))

-- | Compute the positive difference between x and y.
fdimf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fdimf = opify (Op False "fdimf" :: Op (TwoArg F F) (OneArg F))

-- | Divide two floating point values.
fdividef :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fdividef = opify (Op False "fdividef" :: Op (TwoArg F F) (OneArg F))

-- | Calculate the largest integer less than or equal to x.
floorf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
floorf = opify (Op False "floorf" :: Op (OneArg F) (OneArg F))

-- | Compute x × y + z as a single operation.
fmaf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fmaf = opify (Op False "fmaf" :: Op (ThreeArg F F F) (OneArg F))

-- | Determine the maximum numeric value of the arguments.
fmaxf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fmaxf = opify (Op False "fmaxf" :: Op (TwoArg F F) (OneArg F))

-- | Determine the minimum numeric value of the arguments.
fminf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fminf = opify (Op False "fminf" :: Op (TwoArg F F) (OneArg F))

-- | Calculate the floating-point remainder of x / y.
fmodf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fmodf = opify (Op False "fmodf" :: Op (TwoArg F F) (OneArg F))

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float frexpf ( float  x, int* nptr )
-- | 	    Extract mantissa and exponent of a floating-point value. 

-- | Calculate the square root of the sum of squares of two arguments.
hypotf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
hypotf = opify (Op False "hypotf" :: Op (TwoArg F F) (OneArg F))

-- | Compute the unbiased integer exponent of the argument.
ilogbf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
ilogbf = opify (Op False "ilogbf" :: Op (OneArg F) (OneArg I))

-- | Determine whether argument is finite.
isfinite :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
isfinite = opify (Op False "isfinite" :: Op (OneArg F) (OneArg I))

-- | Determine whether argument is infinite.
isinf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
isinf = opify (Op False "isinf" :: Op (OneArg F) (OneArg I))

-- | Determine whether argument is a NaN.
isnan :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
isnan = opify (Op False "isnan" :: Op (OneArg F) (OneArg I))

-- | Calculate the value of the Bessel function of the first kind of order 0 for the input argument.
j0f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
j0f = opify (Op False "j0f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the first kind of order 1 for the input argument.
j1f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
j1f = opify (Op False "j1f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the first kind of order n for the input argument.
jnf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
jnf = opify (Op False "jnf" :: Op (TwoArg I F) (OneArg F))

-- | Calculate the value of x ⋅ 2 e x p .
ldexpf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
ldexpf = opify (Op False "ldexpf" :: Op (TwoArg F I) (OneArg F))

-- | Calculate the natural logarithm of the absolute value of the gamma function of the input argument.
lgammaf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
lgammaf = opify (Op False "lgammaf" :: Op (OneArg F) (OneArg F))

-- | Round input to nearest integer value.
llrintf :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
llrintf = opify (Op False "llrintf" :: Op (OneArg F) (OneArg L))

-- | Round to nearest integer value.
llroundf :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
llroundf = opify (Op False "llroundf" :: Op (OneArg F) (OneArg L))

-- | Calculate the base 10 logarithm of the input argument.
log10f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
log10f = opify (Op False "log10f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of l o g e ( 1 + x ) .
log1pf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
log1pf = opify (Op False "log1pf" :: Op (OneArg F) (OneArg F))

-- | Calculate the base 2 logarithm of the input argument.
log2f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
log2f = opify (Op False "log2f" :: Op (OneArg F) (OneArg F))

-- | Calculate the floating point representation of the exponent of the input argument.
logbf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
logbf = opify (Op False "logbf" :: Op (OneArg F) (OneArg F))

-- | Calculate the natural logarithm of the input argument.
logf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
logf = opify (Op False "logf" :: Op (OneArg F) (OneArg F))

-- | Round input to nearest integer value.
lrintf :: (HasIdentifier w, HasStackLens w F, HasStackLens w LI) => PartialStackOp w
lrintf = opify (Op False "lrintf" :: Op (OneArg F) (OneArg LI))

-- | Round to nearest integer value.
lroundf :: (HasIdentifier w, HasStackLens w F, HasStackLens w LI) => PartialStackOp w
lroundf = opify (Op False "lroundf" :: Op (OneArg F) (OneArg LI))

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float modff ( float  x, float* iptr )
-- | 	    Break down the input argument into fractional and integral parts. 

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float nanf ( const char* tagp )
-- | 	    Returns "Not a Number" value. 

-- | Round the input argument to the nearest integer.
nearbyintf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
nearbyintf = opify (Op False "nearbyintf" :: Op (OneArg F) (OneArg F))

-- | Return next representable single-precision floating-point value afer argument.
nextafterf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
nextafterf = opify (Op False "nextafterf" :: Op (TwoArg F F) (OneArg F))

-- | Calculate the square root of the sum of squares of three coordinates of the argument.
norm3df :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
norm3df = opify (Op False "norm3df" :: Op (ThreeArg F F F) (OneArg F))

-- | Calculate the square root of the sum of squares of four coordinates of the argument.
norm4df :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
norm4df = opify (Op False "norm4df" :: Op (FourArg F F F F) (OneArg F))

-- | Calculate the standard normal cumulative distribution function.
normcdff :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
normcdff = opify (Op False "normcdff" :: Op (OneArg F) (OneArg F))

-- | Calculate the inverse of the standard normal cumulative distribution function.
normcdfinvf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
normcdfinvf = opify (Op False "normcdfinvf" :: Op (OneArg F) (OneArg F))

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float normf ( int  dim, const float* a )
-- | 	    Calculate the square root of the sum of squares of any number of coordinates. 

-- | Calculate the value of first argument to the power of second argument.
powf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
powf = opify (Op False "powf" :: Op (TwoArg F F) (OneArg F))

-- | Calculate reciprocal cube root function.
rcbrtf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rcbrtf = opify (Op False "rcbrtf" :: Op (OneArg F) (OneArg F))

-- | Compute single-precision floating-point remainder.
remainderf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
remainderf = opify (Op False "remainderf" :: Op (TwoArg F F) (OneArg F))

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float remquof ( float  x, float  y, int* quo )
-- | 	    Compute single-precision floating-point remainder and part of quotient. 

-- | Calculate one over the square root of the sum of squares of two arguments.
rhypotf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rhypotf = opify (Op False "rhypotf" :: Op (TwoArg F F) (OneArg F))

-- | Round input to nearest integer value in floating-point.
rintf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rintf = opify (Op False "rintf" :: Op (OneArg F) (OneArg F))

-- | Calculate one over the square root of the sum of squares of three coordinates of the argument.
rnorm3df :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rnorm3df = opify (Op False "rnorm3df" :: Op (ThreeArg F F F) (OneArg F))

-- | Calculate one over the square root of the sum of squares of four coordinates of the argument.
rnorm4df :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rnorm4df = opify (Op False "rnorm4df" :: Op (FourArg F F F F) (OneArg F))

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float rnormf ( int  dim, const float* a )
-- | 	    Calculate the reciprocal of square root of the sum of squares of any number of coordinates. 

-- | Round to nearest integer value in floating-point.
roundf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
roundf = opify (Op False "roundf" :: Op (OneArg F) (OneArg F))

-- | Calculate the reciprocal of the square root of the input argument.
rsqrtf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rsqrtf = opify (Op False "rsqrtf" :: Op (OneArg F) (OneArg F))

-- | Scale floating-point input by integer power of two.
scalblnf :: (HasIdentifier w, HasStackLens w F, HasStackLens w LI) => PartialStackOp w
scalblnf = opify (Op False "scalblnf" :: Op (TwoArg F LI) (OneArg F))

-- | Scale floating-point input by integer power of two.
scalbnf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
scalbnf = opify (Op False "scalbnf" :: Op (TwoArg F I) (OneArg F))

-- | Return the sign bit of the input.
signbit :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
signbit = opify (Op False "signbit" :: Op (OneArg F) (OneArg I))

-- | Failed to parse (Has Void Type!):
-- |
-- | 	__device__   void sincosf ( float  x, float* sptr, float* cptr )
-- | 	    Calculate the sine and cosine of the first input argument. 

-- | Failed to parse (Has Void Type!):
-- |
-- | 	__device__   void sincospif ( float  x, float* sptr, float* cptr )
-- | 	    Calculate the sine and cosine of the first input argument × π . 

-- | Calculate the sine of the input argument.
sinf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
sinf = opify (Op False "sinf" :: Op (OneArg F) (OneArg F))

-- | Calculate the hyperbolic sine of the input argument.
sinhf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
sinhf = opify (Op False "sinhf" :: Op (OneArg F) (OneArg F))

-- | Calculate the sine of the input argument × π .
sinpif :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
sinpif = opify (Op False "sinpif" :: Op (OneArg F) (OneArg F))

-- | Calculate the square root of the input argument.
sqrtf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
sqrtf = opify (Op False "sqrtf" :: Op (OneArg F) (OneArg F))

-- | Calculate the tangent of the input argument.
tanf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
tanf = opify (Op False "tanf" :: Op (OneArg F) (OneArg F))

-- | Calculate the hyperbolic tangent of the input argument.
tanhf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
tanhf = opify (Op False "tanhf" :: Op (OneArg F) (OneArg F))

-- | Calculate the gamma function of the input argument.
tgammaf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
tgammaf = opify (Op False "tgammaf" :: Op (OneArg F) (OneArg F))

-- | Truncate input argument to the integral part.
truncf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
truncf = opify (Op False "truncf" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the second kind of order 0 for the input argument.
y0f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
y0f = opify (Op False "y0f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the second kind of order 1 for the input argument.
y1f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
y1f = opify (Op False "y1f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the second kind of order n for the input argument.
ynf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
ynf = opify (Op False "ynf" :: Op (TwoArg I F) (OneArg F))

