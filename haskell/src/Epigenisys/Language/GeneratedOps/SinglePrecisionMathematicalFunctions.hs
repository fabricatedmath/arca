module Epigenisys.Language.GeneratedOps.SinglePrecisionMathematicalFunctions where

import Something

-- | Exported Lists

f :: (HasStackLens w F) => [State w ()]
f = [acosf, acoshf, asinf, asinhf, atan2f, atanf, atanhf, cbrtf, ceilf, copysignf, cosf, coshf, cospif, cyl_bessel_i0f, cyl_bessel_i1f, erfcf, erfcinvf, erfcxf, erff, erfinvf, exp10f, exp2f, expf, expm1f, fabsf, fdimf, fdividef, floorf, fmaf, fmaxf, fminf, fmodf, hypotf, j0f, j1f, lgammaf, log10f, log1pf, log2f, logbf, logf]

fi :: (HasStackLens w F, HasStackLens w I) => [State w ()]
fi = [ilogbf, isfinite, isinf, isnan, jnf, ldexpf]

fl :: (HasStackLens w F, HasStackLens w L) => [State w ()]
fl = [llrintf, llroundf]

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

