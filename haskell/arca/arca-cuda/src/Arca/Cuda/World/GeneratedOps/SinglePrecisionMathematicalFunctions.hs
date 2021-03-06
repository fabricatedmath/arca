{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.GeneratedOps.SinglePrecisionMathematicalFunctions where

import Arca.Language
import Arca.Cuda.World.Internal

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
acosf = opify (Op "acosf" :: Op (OneArg F) F)

-- | Calculate the nonnegative arc hyperbolic cosine of the input argument.
acoshf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
acoshf = opify (Op "acoshf" :: Op (OneArg F) F)

-- | Calculate the arc sine of the input argument.
asinf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
asinf = opify (Op "asinf" :: Op (OneArg F) F)

-- | Calculate the arc hyperbolic sine of the input argument.
asinhf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
asinhf = opify (Op "asinhf" :: Op (OneArg F) F)

-- | Calculate the arc tangent of the ratio of first and second input arguments.
atan2f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
atan2f = opify (Op "atan2f" :: Op (TwoArg F F) F)

-- | Calculate the arc tangent of the input argument.
atanf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
atanf = opify (Op "atanf" :: Op (OneArg F) F)

-- | Calculate the arc hyperbolic tangent of the input argument.
atanhf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
atanhf = opify (Op "atanhf" :: Op (OneArg F) F)

-- | Calculate the cube root of the input argument.
cbrtf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cbrtf = opify (Op "cbrtf" :: Op (OneArg F) F)

-- | Calculate ceiling of the input argument.
ceilf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
ceilf = opify (Op "ceilf" :: Op (OneArg F) F)

-- | Create value with given magnitude, copying sign of second value.
copysignf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
copysignf = opify (Op "copysignf" :: Op (TwoArg F F) F)

-- | Calculate the cosine of the input argument.
cosf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cosf = opify (Op "cosf" :: Op (OneArg F) F)

-- | Calculate the hyperbolic cosine of the input argument.
coshf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
coshf = opify (Op "coshf" :: Op (OneArg F) F)

-- | Calculate the cosine of the input argument × π .
cospif :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cospif = opify (Op "cospif" :: Op (OneArg F) F)

-- | Calculate the value of the regular modified cylindrical Bessel function of order 0 for the input argument.
cyl_bessel_i0f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cyl_bessel_i0f = opify (Op "cyl_bessel_i0f" :: Op (OneArg F) F)

-- | Calculate the value of the regular modified cylindrical Bessel function of order 1 for the input argument.
cyl_bessel_i1f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
cyl_bessel_i1f = opify (Op "cyl_bessel_i1f" :: Op (OneArg F) F)

-- | Calculate the complementary error function of the input argument.
erfcf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erfcf = opify (Op "erfcf" :: Op (OneArg F) F)

-- | Calculate the inverse complementary error function of the input argument.
erfcinvf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erfcinvf = opify (Op "erfcinvf" :: Op (OneArg F) F)

-- | Calculate the scaled complementary error function of the input argument.
erfcxf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erfcxf = opify (Op "erfcxf" :: Op (OneArg F) F)

-- | Calculate the error function of the input argument.
erff :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erff = opify (Op "erff" :: Op (OneArg F) F)

-- | Calculate the inverse error function of the input argument.
erfinvf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
erfinvf = opify (Op "erfinvf" :: Op (OneArg F) F)

-- | Calculate the base 10 exponential of the input argument.
exp10f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
exp10f = opify (Op "exp10f" :: Op (OneArg F) F)

-- | Calculate the base 2 exponential of the input argument.
exp2f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
exp2f = opify (Op "exp2f" :: Op (OneArg F) F)

-- | Calculate the base e exponential of the input argument.
expf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
expf = opify (Op "expf" :: Op (OneArg F) F)

-- | Calculate the base e exponential of the input argument, minus 1.
expm1f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
expm1f = opify (Op "expm1f" :: Op (OneArg F) F)

-- | Calculate the absolute value of its argument.
fabsf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fabsf = opify (Op "fabsf" :: Op (OneArg F) F)

-- | Compute the positive difference between x and y.
fdimf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fdimf = opify (Op "fdimf" :: Op (TwoArg F F) F)

-- | Divide two floating point values.
fdividef :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fdividef = opify (Op "fdividef" :: Op (TwoArg F F) F)

-- | Calculate the largest integer less than or equal to x.
floorf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
floorf = opify (Op "floorf" :: Op (OneArg F) F)

-- | Compute x × y + z as a single operation.
fmaf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fmaf = opify (Op "fmaf" :: Op (ThreeArg F F F) F)

-- | Determine the maximum numeric value of the arguments.
fmaxf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fmaxf = opify (Op "fmaxf" :: Op (TwoArg F F) F)

-- | Determine the minimum numeric value of the arguments.
fminf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fminf = opify (Op "fminf" :: Op (TwoArg F F) F)

-- | Calculate the floating-point remainder of x / y.
fmodf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
fmodf = opify (Op "fmodf" :: Op (TwoArg F F) F)

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float frexpf ( float  x, int* nptr )
-- | 	    Extract mantissa and exponent of a floating-point value. 

-- | Calculate the square root of the sum of squares of two arguments.
hypotf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
hypotf = opify (Op "hypotf" :: Op (TwoArg F F) F)

-- | Compute the unbiased integer exponent of the argument.
ilogbf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
ilogbf = opify (Op "ilogbf" :: Op (OneArg F) I)

-- | Determine whether argument is finite.
isfinite :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
isfinite = opify (Op "isfinite" :: Op (OneArg F) I)

-- | Determine whether argument is infinite.
isinf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
isinf = opify (Op "isinf" :: Op (OneArg F) I)

-- | Determine whether argument is a NaN.
isnan :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
isnan = opify (Op "isnan" :: Op (OneArg F) I)

-- | Calculate the value of the Bessel function of the first kind of order 0 for the input argument.
j0f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
j0f = opify (Op "j0f" :: Op (OneArg F) F)

-- | Calculate the value of the Bessel function of the first kind of order 1 for the input argument.
j1f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
j1f = opify (Op "j1f" :: Op (OneArg F) F)

-- | Calculate the value of the Bessel function of the first kind of order n for the input argument.
jnf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
jnf = opify (Op "jnf" :: Op (TwoArg I F) F)

-- | Calculate the value of x ⋅ 2 e x p .
ldexpf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
ldexpf = opify (Op "ldexpf" :: Op (TwoArg F I) F)

-- | Calculate the natural logarithm of the absolute value of the gamma function of the input argument.
lgammaf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
lgammaf = opify (Op "lgammaf" :: Op (OneArg F) F)

-- | Round input to nearest integer value.
llrintf :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
llrintf = opify (Op "llrintf" :: Op (OneArg F) L)

-- | Round to nearest integer value.
llroundf :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
llroundf = opify (Op "llroundf" :: Op (OneArg F) L)

-- | Calculate the base 10 logarithm of the input argument.
log10f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
log10f = opify (Op "log10f" :: Op (OneArg F) F)

-- | Calculate the value of l o g e ( 1 + x ) .
log1pf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
log1pf = opify (Op "log1pf" :: Op (OneArg F) F)

-- | Calculate the base 2 logarithm of the input argument.
log2f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
log2f = opify (Op "log2f" :: Op (OneArg F) F)

-- | Calculate the floating point representation of the exponent of the input argument.
logbf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
logbf = opify (Op "logbf" :: Op (OneArg F) F)

-- | Calculate the natural logarithm of the input argument.
logf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
logf = opify (Op "logf" :: Op (OneArg F) F)

-- | Round input to nearest integer value.
lrintf :: (HasIdentifier w, HasStackLens w F, HasStackLens w LI) => PartialStackOp w
lrintf = opify (Op "lrintf" :: Op (OneArg F) LI)

-- | Round to nearest integer value.
lroundf :: (HasIdentifier w, HasStackLens w F, HasStackLens w LI) => PartialStackOp w
lroundf = opify (Op "lroundf" :: Op (OneArg F) LI)

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
nearbyintf = opify (Op "nearbyintf" :: Op (OneArg F) F)

-- | Return next representable single-precision floating-point value afer argument.
nextafterf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
nextafterf = opify (Op "nextafterf" :: Op (TwoArg F F) F)

-- | Calculate the square root of the sum of squares of three coordinates of the argument.
norm3df :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
norm3df = opify (Op "norm3df" :: Op (ThreeArg F F F) F)

-- | Calculate the square root of the sum of squares of four coordinates of the argument.
norm4df :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
norm4df = opify (Op "norm4df" :: Op (FourArg F F F F) F)

-- | Calculate the standard normal cumulative distribution function.
normcdff :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
normcdff = opify (Op "normcdff" :: Op (OneArg F) F)

-- | Calculate the inverse of the standard normal cumulative distribution function.
normcdfinvf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
normcdfinvf = opify (Op "normcdfinvf" :: Op (OneArg F) F)

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float normf ( int  dim, const float* a )
-- | 	    Calculate the square root of the sum of squares of any number of coordinates. 

-- | Calculate the value of first argument to the power of second argument.
powf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
powf = opify (Op "powf" :: Op (TwoArg F F) F)

-- | Calculate reciprocal cube root function.
rcbrtf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rcbrtf = opify (Op "rcbrtf" :: Op (OneArg F) F)

-- | Compute single-precision floating-point remainder.
remainderf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
remainderf = opify (Op "remainderf" :: Op (TwoArg F F) F)

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float remquof ( float  x, float  y, int* quo )
-- | 	    Compute single-precision floating-point remainder and part of quotient. 

-- | Calculate one over the square root of the sum of squares of two arguments.
rhypotf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rhypotf = opify (Op "rhypotf" :: Op (TwoArg F F) F)

-- | Round input to nearest integer value in floating-point.
rintf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rintf = opify (Op "rintf" :: Op (OneArg F) F)

-- | Calculate one over the square root of the sum of squares of three coordinates of the argument.
rnorm3df :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rnorm3df = opify (Op "rnorm3df" :: Op (ThreeArg F F F) F)

-- | Calculate one over the square root of the sum of squares of four coordinates of the argument.
rnorm4df :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rnorm4df = opify (Op "rnorm4df" :: Op (FourArg F F F F) F)

-- | Failed to parse (Has Pointer Type!):
-- |
-- | 	__device__   float rnormf ( int  dim, const float* a )
-- | 	    Calculate the reciprocal of square root of the sum of squares of any number of coordinates. 

-- | Round to nearest integer value in floating-point.
roundf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
roundf = opify (Op "roundf" :: Op (OneArg F) F)

-- | Calculate the reciprocal of the square root of the input argument.
rsqrtf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
rsqrtf = opify (Op "rsqrtf" :: Op (OneArg F) F)

-- | Scale floating-point input by integer power of two.
scalblnf :: (HasIdentifier w, HasStackLens w F, HasStackLens w LI) => PartialStackOp w
scalblnf = opify (Op "scalblnf" :: Op (TwoArg F LI) F)

-- | Scale floating-point input by integer power of two.
scalbnf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
scalbnf = opify (Op "scalbnf" :: Op (TwoArg F I) F)

-- | Return the sign bit of the input.
signbit :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
signbit = opify (Op "signbit" :: Op (OneArg F) I)

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
sinf = opify (Op "sinf" :: Op (OneArg F) F)

-- | Calculate the hyperbolic sine of the input argument.
sinhf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
sinhf = opify (Op "sinhf" :: Op (OneArg F) F)

-- | Calculate the sine of the input argument × π .
sinpif :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
sinpif = opify (Op "sinpif" :: Op (OneArg F) F)

-- | Calculate the square root of the input argument.
sqrtf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
sqrtf = opify (Op "sqrtf" :: Op (OneArg F) F)

-- | Calculate the tangent of the input argument.
tanf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
tanf = opify (Op "tanf" :: Op (OneArg F) F)

-- | Calculate the hyperbolic tangent of the input argument.
tanhf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
tanhf = opify (Op "tanhf" :: Op (OneArg F) F)

-- | Calculate the gamma function of the input argument.
tgammaf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
tgammaf = opify (Op "tgammaf" :: Op (OneArg F) F)

-- | Truncate input argument to the integral part.
truncf :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
truncf = opify (Op "truncf" :: Op (OneArg F) F)

-- | Calculate the value of the Bessel function of the second kind of order 0 for the input argument.
y0f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
y0f = opify (Op "y0f" :: Op (OneArg F) F)

-- | Calculate the value of the Bessel function of the second kind of order 1 for the input argument.
y1f :: (HasIdentifier w, HasStackLens w F) => PartialStackOp w
y1f = opify (Op "y1f" :: Op (OneArg F) F)

-- | Calculate the value of the Bessel function of the second kind of order n for the input argument.
ynf :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
ynf = opify (Op "ynf" :: Op (TwoArg I F) F)

