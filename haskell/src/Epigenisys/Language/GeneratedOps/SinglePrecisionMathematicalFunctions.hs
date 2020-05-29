module Epigenisys.Language.GeneratedOps.SinglePrecisionMathematicalFunctions where

import Something

-- | Exported Lists

f :: (HasStackLens w F) => [State w ()]
f = [acosf, acoshf, asinf, asinhf, atan2f, atanf, atanhf, cbrtf, ceilf, copysignf, cosf, coshf, cospif, cyl_bessel_i0f, cyl_bessel_i1f, erfcf, erfcinvf, erfcxf, erff, erfinvf, exp10f, exp2f, expf, expm1f, fabsf, fdimf, fdividef, floorf, fmaf, fmaxf, fminf, fmodf, hypotf, j0f, j1f, lgammaf, log10f, log1pf, log2f, logbf, logf, nearbyintf, nextafterf, norm3df, norm4df, normcdff, normcdfinvf, powf, rcbrtf, remainderf, rhypotf, rintf, rnorm3df, rnorm4df, roundf, rsqrtf, sinf, sinhf, sinpif, sqrtf, tanf, tanhf, tgammaf, truncf, y0f, y1f]

f_i :: (HasStackLens w F, HasStackLens w I) => [State w ()]
f_i = [ilogbf, isfinite, isinf, isnan, jnf, ldexpf, scalbnf, signbit, ynf]

f_li :: (HasStackLens w F, HasStackLens w LI) => [State w ()]
f_li = [lrintf, lroundf, scalblnf]

f_l :: (HasStackLens w F, HasStackLens w L) => [State w ()]
f_l = [llrintf, llroundf]

-- | Exported Ops

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

-- | Create value with given magnitude, copying sign of second value.
copysignf :: (HasStackLens w F) => State w ()
copysignf = opify (Op False "copysignf" :: Op (OneArg F) (TwoArg F F))

-- | Calculate the cosine of the input argument.
cosf :: (HasStackLens w F) => State w ()
cosf = opify (Op False "cosf" :: Op (OneArg F) (OneArg F))

-- | Calculate the hyperbolic cosine of the input argument.
coshf :: (HasStackLens w F) => State w ()
coshf = opify (Op False "coshf" :: Op (OneArg F) (OneArg F))

-- | Calculate the cosine of the input argument × π .
cospif :: (HasStackLens w F) => State w ()
cospif = opify (Op False "cospif" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the regular modified cylindrical Bessel function of order 0 for the input argument.
cyl_bessel_i0f :: (HasStackLens w F) => State w ()
cyl_bessel_i0f = opify (Op False "cyl_bessel_i0f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the regular modified cylindrical Bessel function of order 1 for the input argument.
cyl_bessel_i1f :: (HasStackLens w F) => State w ()
cyl_bessel_i1f = opify (Op False "cyl_bessel_i1f" :: Op (OneArg F) (OneArg F))

-- | Calculate the complementary error function of the input argument.
erfcf :: (HasStackLens w F) => State w ()
erfcf = opify (Op False "erfcf" :: Op (OneArg F) (OneArg F))

-- | Calculate the inverse complementary error function of the input argument.
erfcinvf :: (HasStackLens w F) => State w ()
erfcinvf = opify (Op False "erfcinvf" :: Op (OneArg F) (OneArg F))

-- | Calculate the scaled complementary error function of the input argument.
erfcxf :: (HasStackLens w F) => State w ()
erfcxf = opify (Op False "erfcxf" :: Op (OneArg F) (OneArg F))

-- | Calculate the error function of the input argument.
erff :: (HasStackLens w F) => State w ()
erff = opify (Op False "erff" :: Op (OneArg F) (OneArg F))

-- | Calculate the inverse error function of the input argument.
erfinvf :: (HasStackLens w F) => State w ()
erfinvf = opify (Op False "erfinvf" :: Op (OneArg F) (OneArg F))

-- | Calculate the base 10 exponential of the input argument.
exp10f :: (HasStackLens w F) => State w ()
exp10f = opify (Op False "exp10f" :: Op (OneArg F) (OneArg F))

-- | Calculate the base 2 exponential of the input argument.
exp2f :: (HasStackLens w F) => State w ()
exp2f = opify (Op False "exp2f" :: Op (OneArg F) (OneArg F))

-- | Calculate the base e exponential of the input argument.
expf :: (HasStackLens w F) => State w ()
expf = opify (Op False "expf" :: Op (OneArg F) (OneArg F))

-- | Calculate the base e exponential of the input argument, minus 1.
expm1f :: (HasStackLens w F) => State w ()
expm1f = opify (Op False "expm1f" :: Op (OneArg F) (OneArg F))

-- | Calculate the absolute value of its argument.
fabsf :: (HasStackLens w F) => State w ()
fabsf = opify (Op False "fabsf" :: Op (OneArg F) (OneArg F))

-- | Compute the positive difference between x and y.
fdimf :: (HasStackLens w F) => State w ()
fdimf = opify (Op False "fdimf" :: Op (OneArg F) (TwoArg F F))

-- | Divide two floating point values.
fdividef :: (HasStackLens w F) => State w ()
fdividef = opify (Op False "fdividef" :: Op (OneArg F) (TwoArg F F))

-- | Calculate the largest integer less than or equal to x.
floorf :: (HasStackLens w F) => State w ()
floorf = opify (Op False "floorf" :: Op (OneArg F) (OneArg F))

-- | Compute x × y + z as a single operation.
fmaf :: (HasStackLens w F) => State w ()
fmaf = opify (Op False "fmaf" :: Op (OneArg F) (ThreeArg F F F))

-- | Determine the maximum numeric value of the arguments.
fmaxf :: (HasStackLens w F) => State w ()
fmaxf = opify (Op False "fmaxf" :: Op (OneArg F) (TwoArg F F))

-- | Determine the minimum numeric value of the arguments.
fminf :: (HasStackLens w F) => State w ()
fminf = opify (Op False "fminf" :: Op (OneArg F) (TwoArg F F))

-- | Calculate the floating-point remainder of x / y.
fmodf :: (HasStackLens w F) => State w ()
fmodf = opify (Op False "fmodf" :: Op (OneArg F) (TwoArg F F))

-- | Failed to parse (Has Pointer Type):
-- |
-- | 	__device__   float frexpf ( float  x, int* nptr )
-- | 	    Extract mantissa and exponent of a floating-point value. 

-- | Calculate the square root of the sum of squares of two arguments.
hypotf :: (HasStackLens w F) => State w ()
hypotf = opify (Op False "hypotf" :: Op (OneArg F) (TwoArg F F))

-- | Compute the unbiased integer exponent of the argument.
ilogbf :: (HasStackLens w F, HasStackLens w I) => State w ()
ilogbf = opify (Op False "ilogbf" :: Op (OneArg I) (OneArg F))

-- | Determine whether argument is finite.
isfinite :: (HasStackLens w F, HasStackLens w I) => State w ()
isfinite = opify (Op False "isfinite" :: Op (OneArg I) (OneArg F))

-- | Determine whether argument is infinite.
isinf :: (HasStackLens w F, HasStackLens w I) => State w ()
isinf = opify (Op False "isinf" :: Op (OneArg I) (OneArg F))

-- | Determine whether argument is a NaN.
isnan :: (HasStackLens w F, HasStackLens w I) => State w ()
isnan = opify (Op False "isnan" :: Op (OneArg I) (OneArg F))

-- | Calculate the value of the Bessel function of the first kind of order 0 for the input argument.
j0f :: (HasStackLens w F) => State w ()
j0f = opify (Op False "j0f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the first kind of order 1 for the input argument.
j1f :: (HasStackLens w F) => State w ()
j1f = opify (Op False "j1f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the first kind of order n for the input argument.
jnf :: (HasStackLens w F, HasStackLens w I) => State w ()
jnf = opify (Op False "jnf" :: Op (OneArg F) (TwoArg I F))

-- | Calculate the value of x ⋅ 2 e x p .
ldexpf :: (HasStackLens w F, HasStackLens w I) => State w ()
ldexpf = opify (Op False "ldexpf" :: Op (OneArg F) (TwoArg F I))

-- | Calculate the natural logarithm of the absolute value of the gamma function of the input argument.
lgammaf :: (HasStackLens w F) => State w ()
lgammaf = opify (Op False "lgammaf" :: Op (OneArg F) (OneArg F))

-- | Round input to nearest integer value.
llrintf :: (HasStackLens w F, HasStackLens w L) => State w ()
llrintf = opify (Op False "llrintf" :: Op (OneArg L) (OneArg F))

-- | Round to nearest integer value.
llroundf :: (HasStackLens w F, HasStackLens w L) => State w ()
llroundf = opify (Op False "llroundf" :: Op (OneArg L) (OneArg F))

-- | Calculate the base 10 logarithm of the input argument.
log10f :: (HasStackLens w F) => State w ()
log10f = opify (Op False "log10f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of l o g e ( 1 + x ) .
log1pf :: (HasStackLens w F) => State w ()
log1pf = opify (Op False "log1pf" :: Op (OneArg F) (OneArg F))

-- | Calculate the base 2 logarithm of the input argument.
log2f :: (HasStackLens w F) => State w ()
log2f = opify (Op False "log2f" :: Op (OneArg F) (OneArg F))

-- | Calculate the floating point representation of the exponent of the input argument.
logbf :: (HasStackLens w F) => State w ()
logbf = opify (Op False "logbf" :: Op (OneArg F) (OneArg F))

-- | Calculate the natural logarithm of the input argument.
logf :: (HasStackLens w F) => State w ()
logf = opify (Op False "logf" :: Op (OneArg F) (OneArg F))

-- | Round input to nearest integer value.
lrintf :: (HasStackLens w F, HasStackLens w LI) => State w ()
lrintf = opify (Op False "lrintf" :: Op (OneArg LI) (OneArg F))

-- | Round to nearest integer value.
lroundf :: (HasStackLens w F, HasStackLens w LI) => State w ()
lroundf = opify (Op False "lroundf" :: Op (OneArg LI) (OneArg F))

-- | Failed to parse (Has Pointer Type):
-- |
-- | 	__device__   float modff ( float  x, float* iptr )
-- | 	    Break down the input argument into fractional and integral parts. 

-- | Failed to parse (Has Pointer Type):
-- |
-- | 	__device__   float nanf ( const char* tagp )
-- | 	    Returns "Not a Number" value. 

-- | Round the input argument to the nearest integer.
nearbyintf :: (HasStackLens w F) => State w ()
nearbyintf = opify (Op False "nearbyintf" :: Op (OneArg F) (OneArg F))

-- | Return next representable single-precision floating-point value afer argument.
nextafterf :: (HasStackLens w F) => State w ()
nextafterf = opify (Op False "nextafterf" :: Op (OneArg F) (TwoArg F F))

-- | Calculate the square root of the sum of squares of three coordinates of the argument.
norm3df :: (HasStackLens w F) => State w ()
norm3df = opify (Op False "norm3df" :: Op (OneArg F) (ThreeArg F F F))

-- | Calculate the square root of the sum of squares of four coordinates of the argument.
norm4df :: (HasStackLens w F) => State w ()
norm4df = opify (Op False "norm4df" :: Op (OneArg F) (FourArg F F F F))

-- | Calculate the standard normal cumulative distribution function.
normcdff :: (HasStackLens w F) => State w ()
normcdff = opify (Op False "normcdff" :: Op (OneArg F) (OneArg F))

-- | Calculate the inverse of the standard normal cumulative distribution function.
normcdfinvf :: (HasStackLens w F) => State w ()
normcdfinvf = opify (Op False "normcdfinvf" :: Op (OneArg F) (OneArg F))

-- | Failed to parse (Has Pointer Type):
-- |
-- | 	__device__   float normf ( int  dim, const float* a )
-- | 	    Calculate the square root of the sum of squares of any number of coordinates. 

-- | Calculate the value of first argument to the power of second argument.
powf :: (HasStackLens w F) => State w ()
powf = opify (Op False "powf" :: Op (OneArg F) (TwoArg F F))

-- | Calculate reciprocal cube root function.
rcbrtf :: (HasStackLens w F) => State w ()
rcbrtf = opify (Op False "rcbrtf" :: Op (OneArg F) (OneArg F))

-- | Compute single-precision floating-point remainder.
remainderf :: (HasStackLens w F) => State w ()
remainderf = opify (Op False "remainderf" :: Op (OneArg F) (TwoArg F F))

-- | Failed to parse (Has Pointer Type):
-- |
-- | 	__device__   float remquof ( float  x, float  y, int* quo )
-- | 	    Compute single-precision floating-point remainder and part of quotient. 

-- | Calculate one over the square root of the sum of squares of two arguments.
rhypotf :: (HasStackLens w F) => State w ()
rhypotf = opify (Op False "rhypotf" :: Op (OneArg F) (TwoArg F F))

-- | Round input to nearest integer value in floating-point.
rintf :: (HasStackLens w F) => State w ()
rintf = opify (Op False "rintf" :: Op (OneArg F) (OneArg F))

-- | Calculate one over the square root of the sum of squares of three coordinates of the argument.
rnorm3df :: (HasStackLens w F) => State w ()
rnorm3df = opify (Op False "rnorm3df" :: Op (OneArg F) (ThreeArg F F F))

-- | Calculate one over the square root of the sum of squares of four coordinates of the argument.
rnorm4df :: (HasStackLens w F) => State w ()
rnorm4df = opify (Op False "rnorm4df" :: Op (OneArg F) (FourArg F F F F))

-- | Failed to parse (Has Pointer Type):
-- |
-- | 	__device__   float rnormf ( int  dim, const float* a )
-- | 	    Calculate the reciprocal of square root of the sum of squares of any number of coordinates. 

-- | Round to nearest integer value in floating-point.
roundf :: (HasStackLens w F) => State w ()
roundf = opify (Op False "roundf" :: Op (OneArg F) (OneArg F))

-- | Calculate the reciprocal of the square root of the input argument.
rsqrtf :: (HasStackLens w F) => State w ()
rsqrtf = opify (Op False "rsqrtf" :: Op (OneArg F) (OneArg F))

-- | Scale floating-point input by integer power of two.
scalblnf :: (HasStackLens w F, HasStackLens w LI) => State w ()
scalblnf = opify (Op False "scalblnf" :: Op (OneArg F) (TwoArg F LI))

-- | Scale floating-point input by integer power of two.
scalbnf :: (HasStackLens w F, HasStackLens w I) => State w ()
scalbnf = opify (Op False "scalbnf" :: Op (OneArg F) (TwoArg F I))

-- | Return the sign bit of the input.
signbit :: (HasStackLens w F, HasStackLens w I) => State w ()
signbit = opify (Op False "signbit" :: Op (OneArg I) (OneArg F))

-- | Failed to parse (Has Void Type):
-- |
-- | 	__device__   void sincosf ( float  x, float* sptr, float* cptr )
-- | 	    Calculate the sine and cosine of the first input argument. 

-- | Failed to parse (Has Void Type):
-- |
-- | 	__device__   void sincospif ( float  x, float* sptr, float* cptr )
-- | 	    Calculate the sine and cosine of the first input argument × π . 

-- | Calculate the sine of the input argument.
sinf :: (HasStackLens w F) => State w ()
sinf = opify (Op False "sinf" :: Op (OneArg F) (OneArg F))

-- | Calculate the hyperbolic sine of the input argument.
sinhf :: (HasStackLens w F) => State w ()
sinhf = opify (Op False "sinhf" :: Op (OneArg F) (OneArg F))

-- | Calculate the sine of the input argument × π .
sinpif :: (HasStackLens w F) => State w ()
sinpif = opify (Op False "sinpif" :: Op (OneArg F) (OneArg F))

-- | Calculate the square root of the input argument.
sqrtf :: (HasStackLens w F) => State w ()
sqrtf = opify (Op False "sqrtf" :: Op (OneArg F) (OneArg F))

-- | Calculate the tangent of the input argument.
tanf :: (HasStackLens w F) => State w ()
tanf = opify (Op False "tanf" :: Op (OneArg F) (OneArg F))

-- | Calculate the hyperbolic tangent of the input argument.
tanhf :: (HasStackLens w F) => State w ()
tanhf = opify (Op False "tanhf" :: Op (OneArg F) (OneArg F))

-- | Calculate the gamma function of the input argument.
tgammaf :: (HasStackLens w F) => State w ()
tgammaf = opify (Op False "tgammaf" :: Op (OneArg F) (OneArg F))

-- | Truncate input argument to the integral part.
truncf :: (HasStackLens w F) => State w ()
truncf = opify (Op False "truncf" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the second kind of order 0 for the input argument.
y0f :: (HasStackLens w F) => State w ()
y0f = opify (Op False "y0f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the second kind of order 1 for the input argument.
y1f :: (HasStackLens w F) => State w ()
y1f = opify (Op False "y1f" :: Op (OneArg F) (OneArg F))

-- | Calculate the value of the Bessel function of the second kind of order n for the input argument.
ynf :: (HasStackLens w F, HasStackLens w I) => State w ()
ynf = opify (Op False "ynf" :: Op (OneArg F) (TwoArg I F))

