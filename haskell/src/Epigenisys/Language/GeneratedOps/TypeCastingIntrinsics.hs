module Epigenisys.Language.GeneratedOps.TypeCastingIntrinsics where

import Something

-- | Exported Lists

f_d :: (HasStackLens w F, HasStackLens w D) => [State w ()]
f_d = [__double2float_rd, __double2float_rn, __double2float_ru, __double2float_rz]

f_i :: (HasStackLens w F, HasStackLens w I) => [State w ()]
f_i = [__float2int_rd, __float2int_rn, __float2int_ru, __float2int_rz, __float_as_int, __int2float_rd, __int2float_rn, __int2float_ru, __int2float_rz, __int_as_float]

f_ui :: (HasStackLens w F, HasStackLens w UI) => [State w ()]
f_ui = [__float2uint_rd, __float2uint_rn, __float2uint_ru, __float2uint_rz, __float_as_uint, __uint2float_rd, __uint2float_rn, __uint2float_ru, __uint2float_rz, __uint_as_float]

f_l :: (HasStackLens w F, HasStackLens w L) => [State w ()]
f_l = [__float2ll_rd, __float2ll_rn, __float2ll_ru, __float2ll_rz, __ll2float_rd, __ll2float_rn, __ll2float_ru, __ll2float_rz]

f_ul :: (HasStackLens w F, HasStackLens w UL) => [State w ()]
f_ul = [__float2ull_rd, __float2ull_rn, __float2ull_ru, __float2ull_rz, __ull2float_rd, __ull2float_rn, __ull2float_ru, __ull2float_rz]

d_i :: (HasStackLens w D, HasStackLens w I) => [State w ()]
d_i = [__double2hiint, __double2int_rd, __double2int_rn, __double2int_ru, __double2int_rz, __double2loint, __hiloint2double, __int2double_rn]

d_ui :: (HasStackLens w D, HasStackLens w UI) => [State w ()]
d_ui = [__double2uint_rd, __double2uint_rn, __double2uint_ru, __double2uint_rz, __uint2double_rn]

d_l :: (HasStackLens w D, HasStackLens w L) => [State w ()]
d_l = [__double2ll_rd, __double2ll_rn, __double2ll_ru, __double2ll_rz, __double_as_longlong, __ll2double_rd, __ll2double_rn, __ll2double_ru, __ll2double_rz, __longlong_as_double]

d_ul :: (HasStackLens w D, HasStackLens w UL) => [State w ()]
d_ul = [__double2ull_rd, __double2ull_rn, __double2ull_ru, __double2ull_rz, __ull2double_rd, __ull2double_rn, __ull2double_ru, __ull2double_rz]

-- | Exported Ops

-- | Convert a double to a float in round-down mode.
__double2float_rd :: (HasStackLens w F, HasStackLens w D) => State w ()
__double2float_rd = opify (Op False "__double2float_rd" :: Op (OneArg F) (OneArg D))

-- | Convert a double to a float in round-to-nearest-even mode.
__double2float_rn :: (HasStackLens w F, HasStackLens w D) => State w ()
__double2float_rn = opify (Op False "__double2float_rn" :: Op (OneArg F) (OneArg D))

-- | Convert a double to a float in round-up mode.
__double2float_ru :: (HasStackLens w F, HasStackLens w D) => State w ()
__double2float_ru = opify (Op False "__double2float_ru" :: Op (OneArg F) (OneArg D))

-- | Convert a double to a float in round-towards-zero mode.
__double2float_rz :: (HasStackLens w F, HasStackLens w D) => State w ()
__double2float_rz = opify (Op False "__double2float_rz" :: Op (OneArg F) (OneArg D))

-- | Reinterpret high 32 bits in a double as a signed integer.
__double2hiint :: (HasStackLens w D, HasStackLens w I) => State w ()
__double2hiint = opify (Op False "__double2hiint" :: Op (OneArg I) (OneArg D))

-- | Convert a double to a signed int in round-down mode.
__double2int_rd :: (HasStackLens w D, HasStackLens w I) => State w ()
__double2int_rd = opify (Op False "__double2int_rd" :: Op (OneArg I) (OneArg D))

-- | Convert a double to a signed int in round-to-nearest-even mode.
__double2int_rn :: (HasStackLens w D, HasStackLens w I) => State w ()
__double2int_rn = opify (Op False "__double2int_rn" :: Op (OneArg I) (OneArg D))

-- | Convert a double to a signed int in round-up mode.
__double2int_ru :: (HasStackLens w D, HasStackLens w I) => State w ()
__double2int_ru = opify (Op False "__double2int_ru" :: Op (OneArg I) (OneArg D))

-- | Convert a double to a signed int in round-towards-zero mode.
__double2int_rz :: (HasStackLens w D, HasStackLens w I) => State w ()
__double2int_rz = opify (Op False "__double2int_rz" :: Op (OneArg I) (OneArg D))

-- | Convert a double to a signed 64-bit int in round-down mode.
__double2ll_rd :: (HasStackLens w D, HasStackLens w L) => State w ()
__double2ll_rd = opify (Op False "__double2ll_rd" :: Op (OneArg L) (OneArg D))

-- | Convert a double to a signed 64-bit int in round-to-nearest-even mode.
__double2ll_rn :: (HasStackLens w D, HasStackLens w L) => State w ()
__double2ll_rn = opify (Op False "__double2ll_rn" :: Op (OneArg L) (OneArg D))

-- | Convert a double to a signed 64-bit int in round-up mode.
__double2ll_ru :: (HasStackLens w D, HasStackLens w L) => State w ()
__double2ll_ru = opify (Op False "__double2ll_ru" :: Op (OneArg L) (OneArg D))

-- | Convert a double to a signed 64-bit int in round-towards-zero mode.
__double2ll_rz :: (HasStackLens w D, HasStackLens w L) => State w ()
__double2ll_rz = opify (Op False "__double2ll_rz" :: Op (OneArg L) (OneArg D))

-- | Reinterpret low 32 bits in a double as a signed integer.
__double2loint :: (HasStackLens w D, HasStackLens w I) => State w ()
__double2loint = opify (Op False "__double2loint" :: Op (OneArg I) (OneArg D))

-- | Convert a double to an unsigned int in round-down mode.
__double2uint_rd :: (HasStackLens w D, HasStackLens w UI) => State w ()
__double2uint_rd = opify (Op False "__double2uint_rd" :: Op (OneArg UI) (OneArg D))

-- | Convert a double to an unsigned int in round-to-nearest-even mode.
__double2uint_rn :: (HasStackLens w D, HasStackLens w UI) => State w ()
__double2uint_rn = opify (Op False "__double2uint_rn" :: Op (OneArg UI) (OneArg D))

-- | Convert a double to an unsigned int in round-up mode.
__double2uint_ru :: (HasStackLens w D, HasStackLens w UI) => State w ()
__double2uint_ru = opify (Op False "__double2uint_ru" :: Op (OneArg UI) (OneArg D))

-- | Convert a double to an unsigned int in round-towards-zero mode.
__double2uint_rz :: (HasStackLens w D, HasStackLens w UI) => State w ()
__double2uint_rz = opify (Op False "__double2uint_rz" :: Op (OneArg UI) (OneArg D))

-- | Convert a double to an unsigned 64-bit int in round-down mode.
__double2ull_rd :: (HasStackLens w D, HasStackLens w UL) => State w ()
__double2ull_rd = opify (Op False "__double2ull_rd" :: Op (OneArg UL) (OneArg D))

-- | Convert a double to an unsigned 64-bit int in round-to-nearest-even mode.
__double2ull_rn :: (HasStackLens w D, HasStackLens w UL) => State w ()
__double2ull_rn = opify (Op False "__double2ull_rn" :: Op (OneArg UL) (OneArg D))

-- | Convert a double to an unsigned 64-bit int in round-up mode.
__double2ull_ru :: (HasStackLens w D, HasStackLens w UL) => State w ()
__double2ull_ru = opify (Op False "__double2ull_ru" :: Op (OneArg UL) (OneArg D))

-- | Convert a double to an unsigned 64-bit int in round-towards-zero mode.
__double2ull_rz :: (HasStackLens w D, HasStackLens w UL) => State w ()
__double2ull_rz = opify (Op False "__double2ull_rz" :: Op (OneArg UL) (OneArg D))

-- | Reinterpret bits in a double as a 64-bit signed integer.
__double_as_longlong :: (HasStackLens w D, HasStackLens w L) => State w ()
__double_as_longlong = opify (Op False "__double_as_longlong" :: Op (OneArg L) (OneArg D))

-- | Convert a float to a signed integer in round-down mode.
__float2int_rd :: (HasStackLens w F, HasStackLens w I) => State w ()
__float2int_rd = opify (Op False "__float2int_rd" :: Op (OneArg I) (OneArg F))

-- | Convert a float to a signed integer in round-to-nearest-even mode.
__float2int_rn :: (HasStackLens w F, HasStackLens w I) => State w ()
__float2int_rn = opify (Op False "__float2int_rn" :: Op (OneArg I) (OneArg F))

-- | Convert a float to a signed integer in round-up mode.
__float2int_ru :: (HasStackLens w F, HasStackLens w I) => State w ()
__float2int_ru = opify (Op False "__float2int_ru" :: Op (OneArg I) (OneArg F))

-- | Convert a float to a signed integer in round-towards-zero mode.
__float2int_rz :: (HasStackLens w F, HasStackLens w I) => State w ()
__float2int_rz = opify (Op False "__float2int_rz" :: Op (OneArg I) (OneArg F))

-- | Convert a float to a signed 64-bit integer in round-down mode.
__float2ll_rd :: (HasStackLens w F, HasStackLens w L) => State w ()
__float2ll_rd = opify (Op False "__float2ll_rd" :: Op (OneArg L) (OneArg F))

-- | Convert a float to a signed 64-bit integer in round-to-nearest-even mode.
__float2ll_rn :: (HasStackLens w F, HasStackLens w L) => State w ()
__float2ll_rn = opify (Op False "__float2ll_rn" :: Op (OneArg L) (OneArg F))

-- | Convert a float to a signed 64-bit integer in round-up mode.
__float2ll_ru :: (HasStackLens w F, HasStackLens w L) => State w ()
__float2ll_ru = opify (Op False "__float2ll_ru" :: Op (OneArg L) (OneArg F))

-- | Convert a float to a signed 64-bit integer in round-towards-zero mode.
__float2ll_rz :: (HasStackLens w F, HasStackLens w L) => State w ()
__float2ll_rz = opify (Op False "__float2ll_rz" :: Op (OneArg L) (OneArg F))

-- | Convert a float to an unsigned integer in round-down mode.
__float2uint_rd :: (HasStackLens w F, HasStackLens w UI) => State w ()
__float2uint_rd = opify (Op False "__float2uint_rd" :: Op (OneArg UI) (OneArg F))

-- | Convert a float to an unsigned integer in round-to-nearest-even mode.
__float2uint_rn :: (HasStackLens w F, HasStackLens w UI) => State w ()
__float2uint_rn = opify (Op False "__float2uint_rn" :: Op (OneArg UI) (OneArg F))

-- | Convert a float to an unsigned integer in round-up mode.
__float2uint_ru :: (HasStackLens w F, HasStackLens w UI) => State w ()
__float2uint_ru = opify (Op False "__float2uint_ru" :: Op (OneArg UI) (OneArg F))

-- | Convert a float to an unsigned integer in round-towards-zero mode.
__float2uint_rz :: (HasStackLens w F, HasStackLens w UI) => State w ()
__float2uint_rz = opify (Op False "__float2uint_rz" :: Op (OneArg UI) (OneArg F))

-- | Convert a float to an unsigned 64-bit integer in round-down mode.
__float2ull_rd :: (HasStackLens w F, HasStackLens w UL) => State w ()
__float2ull_rd = opify (Op False "__float2ull_rd" :: Op (OneArg UL) (OneArg F))

-- | Convert a float to an unsigned 64-bit integer in round-to-nearest-even mode.
__float2ull_rn :: (HasStackLens w F, HasStackLens w UL) => State w ()
__float2ull_rn = opify (Op False "__float2ull_rn" :: Op (OneArg UL) (OneArg F))

-- | Convert a float to an unsigned 64-bit integer in round-up mode.
__float2ull_ru :: (HasStackLens w F, HasStackLens w UL) => State w ()
__float2ull_ru = opify (Op False "__float2ull_ru" :: Op (OneArg UL) (OneArg F))

-- | Convert a float to an unsigned 64-bit integer in round-towards-zero mode.
__float2ull_rz :: (HasStackLens w F, HasStackLens w UL) => State w ()
__float2ull_rz = opify (Op False "__float2ull_rz" :: Op (OneArg UL) (OneArg F))

-- | Reinterpret bits in a float as a signed integer.
__float_as_int :: (HasStackLens w F, HasStackLens w I) => State w ()
__float_as_int = opify (Op False "__float_as_int" :: Op (OneArg I) (OneArg F))

-- | Reinterpret bits in a float as a unsigned integer.
__float_as_uint :: (HasStackLens w F, HasStackLens w UI) => State w ()
__float_as_uint = opify (Op False "__float_as_uint" :: Op (OneArg UI) (OneArg F))

-- | Reinterpret high and low 32-bit integer values as a double.
__hiloint2double :: (HasStackLens w D, HasStackLens w I) => State w ()
__hiloint2double = opify (Op False "__hiloint2double" :: Op (OneArg D) (TwoArg I I))

-- | Convert a signed int to a double.
__int2double_rn :: (HasStackLens w D, HasStackLens w I) => State w ()
__int2double_rn = opify (Op False "__int2double_rn" :: Op (OneArg D) (OneArg I))

-- | Convert a signed integer to a float in round-down mode.
__int2float_rd :: (HasStackLens w F, HasStackLens w I) => State w ()
__int2float_rd = opify (Op False "__int2float_rd" :: Op (OneArg F) (OneArg I))

-- | Convert a signed integer to a float in round-to-nearest-even mode.
__int2float_rn :: (HasStackLens w F, HasStackLens w I) => State w ()
__int2float_rn = opify (Op False "__int2float_rn" :: Op (OneArg F) (OneArg I))

-- | Convert a signed integer to a float in round-up mode.
__int2float_ru :: (HasStackLens w F, HasStackLens w I) => State w ()
__int2float_ru = opify (Op False "__int2float_ru" :: Op (OneArg F) (OneArg I))

-- | Convert a signed integer to a float in round-towards-zero mode.
__int2float_rz :: (HasStackLens w F, HasStackLens w I) => State w ()
__int2float_rz = opify (Op False "__int2float_rz" :: Op (OneArg F) (OneArg I))

-- | Reinterpret bits in an integer as a float.
__int_as_float :: (HasStackLens w F, HasStackLens w I) => State w ()
__int_as_float = opify (Op False "__int_as_float" :: Op (OneArg F) (OneArg I))

-- | Convert a signed 64-bit int to a double in round-down mode.
__ll2double_rd :: (HasStackLens w D, HasStackLens w L) => State w ()
__ll2double_rd = opify (Op False "__ll2double_rd" :: Op (OneArg D) (OneArg L))

-- | Convert a signed 64-bit int to a double in round-to-nearest-even mode.
__ll2double_rn :: (HasStackLens w D, HasStackLens w L) => State w ()
__ll2double_rn = opify (Op False "__ll2double_rn" :: Op (OneArg D) (OneArg L))

-- | Convert a signed 64-bit int to a double in round-up mode.
__ll2double_ru :: (HasStackLens w D, HasStackLens w L) => State w ()
__ll2double_ru = opify (Op False "__ll2double_ru" :: Op (OneArg D) (OneArg L))

-- | Convert a signed 64-bit int to a double in round-towards-zero mode.
__ll2double_rz :: (HasStackLens w D, HasStackLens w L) => State w ()
__ll2double_rz = opify (Op False "__ll2double_rz" :: Op (OneArg D) (OneArg L))

-- | Convert a signed integer to a float in round-down mode.
__ll2float_rd :: (HasStackLens w F, HasStackLens w L) => State w ()
__ll2float_rd = opify (Op False "__ll2float_rd" :: Op (OneArg F) (OneArg L))

-- | Convert a signed 64-bit integer to a float in round-to-nearest-even mode.
__ll2float_rn :: (HasStackLens w F, HasStackLens w L) => State w ()
__ll2float_rn = opify (Op False "__ll2float_rn" :: Op (OneArg F) (OneArg L))

-- | Convert a signed integer to a float in round-up mode.
__ll2float_ru :: (HasStackLens w F, HasStackLens w L) => State w ()
__ll2float_ru = opify (Op False "__ll2float_ru" :: Op (OneArg F) (OneArg L))

-- | Convert a signed integer to a float in round-towards-zero mode.
__ll2float_rz :: (HasStackLens w F, HasStackLens w L) => State w ()
__ll2float_rz = opify (Op False "__ll2float_rz" :: Op (OneArg F) (OneArg L))

-- | Reinterpret bits in a 64-bit signed integer as a double.
__longlong_as_double :: (HasStackLens w D, HasStackLens w L) => State w ()
__longlong_as_double = opify (Op False "__longlong_as_double" :: Op (OneArg D) (OneArg L))

-- | Convert an unsigned int to a double.
__uint2double_rn :: (HasStackLens w D, HasStackLens w UI) => State w ()
__uint2double_rn = opify (Op False "__uint2double_rn" :: Op (OneArg D) (OneArg UI))

-- | Convert an unsigned integer to a float in round-down mode.
__uint2float_rd :: (HasStackLens w F, HasStackLens w UI) => State w ()
__uint2float_rd = opify (Op False "__uint2float_rd" :: Op (OneArg F) (OneArg UI))

-- | Convert an unsigned integer to a float in round-to-nearest-even mode.
__uint2float_rn :: (HasStackLens w F, HasStackLens w UI) => State w ()
__uint2float_rn = opify (Op False "__uint2float_rn" :: Op (OneArg F) (OneArg UI))

-- | Convert an unsigned integer to a float in round-up mode.
__uint2float_ru :: (HasStackLens w F, HasStackLens w UI) => State w ()
__uint2float_ru = opify (Op False "__uint2float_ru" :: Op (OneArg F) (OneArg UI))

-- | Convert an unsigned integer to a float in round-towards-zero mode.
__uint2float_rz :: (HasStackLens w F, HasStackLens w UI) => State w ()
__uint2float_rz = opify (Op False "__uint2float_rz" :: Op (OneArg F) (OneArg UI))

-- | Reinterpret bits in an unsigned integer as a float.
__uint_as_float :: (HasStackLens w F, HasStackLens w UI) => State w ()
__uint_as_float = opify (Op False "__uint_as_float" :: Op (OneArg F) (OneArg UI))

-- | Convert an unsigned 64-bit int to a double in round-down mode.
__ull2double_rd :: (HasStackLens w D, HasStackLens w UL) => State w ()
__ull2double_rd = opify (Op False "__ull2double_rd" :: Op (OneArg D) (OneArg UL))

-- | Convert an unsigned 64-bit int to a double in round-to-nearest-even mode.
__ull2double_rn :: (HasStackLens w D, HasStackLens w UL) => State w ()
__ull2double_rn = opify (Op False "__ull2double_rn" :: Op (OneArg D) (OneArg UL))

-- | Convert an unsigned 64-bit int to a double in round-up mode.
__ull2double_ru :: (HasStackLens w D, HasStackLens w UL) => State w ()
__ull2double_ru = opify (Op False "__ull2double_ru" :: Op (OneArg D) (OneArg UL))

-- | Convert an unsigned 64-bit int to a double in round-towards-zero mode.
__ull2double_rz :: (HasStackLens w D, HasStackLens w UL) => State w ()
__ull2double_rz = opify (Op False "__ull2double_rz" :: Op (OneArg D) (OneArg UL))

-- | Convert an unsigned integer to a float in round-down mode.
__ull2float_rd :: (HasStackLens w F, HasStackLens w UL) => State w ()
__ull2float_rd = opify (Op False "__ull2float_rd" :: Op (OneArg F) (OneArg UL))

-- | Convert an unsigned integer to a float in round-to-nearest-even mode.
__ull2float_rn :: (HasStackLens w F, HasStackLens w UL) => State w ()
__ull2float_rn = opify (Op False "__ull2float_rn" :: Op (OneArg F) (OneArg UL))

-- | Convert an unsigned integer to a float in round-up mode.
__ull2float_ru :: (HasStackLens w F, HasStackLens w UL) => State w ()
__ull2float_ru = opify (Op False "__ull2float_ru" :: Op (OneArg F) (OneArg UL))

-- | Convert an unsigned integer to a float in round-towards-zero mode.
__ull2float_rz :: (HasStackLens w F, HasStackLens w UL) => State w ()
__ull2float_rz = opify (Op False "__ull2float_rz" :: Op (OneArg F) (OneArg UL))

