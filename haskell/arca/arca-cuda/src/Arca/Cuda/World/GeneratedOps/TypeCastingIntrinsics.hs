{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.GeneratedOps.TypeCastingIntrinsics where

import Arca.Language
import Arca.Cuda.World.Internal

-- | Exported Lists

f_d :: (HasIdentifier w, HasStackLens w F, HasStackLens w D) => [PartialStackOp w]
f_d = [__double2float_rd, __double2float_rn, __double2float_ru, __double2float_rz]

f_i :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => [PartialStackOp w]
f_i = [__float2int_rd, __float2int_rn, __float2int_ru, __float2int_rz, __float_as_int, __int2float_rd, __int2float_rn, __int2float_ru, __int2float_rz, __int_as_float]

f_ui :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => [PartialStackOp w]
f_ui = [__float2uint_rd, __float2uint_rn, __float2uint_ru, __float2uint_rz, __float_as_uint, __uint2float_rd, __uint2float_rn, __uint2float_ru, __uint2float_rz, __uint_as_float]

f_l :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => [PartialStackOp w]
f_l = [__float2ll_rd, __float2ll_rn, __float2ll_ru, __float2ll_rz, __ll2float_rd, __ll2float_rn, __ll2float_ru, __ll2float_rz]

f_ul :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => [PartialStackOp w]
f_ul = [__float2ull_rd, __float2ull_rn, __float2ull_ru, __float2ull_rz, __ull2float_rd, __ull2float_rn, __ull2float_ru, __ull2float_rz]

d_i :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => [PartialStackOp w]
d_i = [__double2hiint, __double2int_rd, __double2int_rn, __double2int_ru, __double2int_rz, __double2loint, __hiloint2double, __int2double_rn]

d_ui :: (HasIdentifier w, HasStackLens w D, HasStackLens w UI) => [PartialStackOp w]
d_ui = [__double2uint_rd, __double2uint_rn, __double2uint_ru, __double2uint_rz, __uint2double_rn]

d_l :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => [PartialStackOp w]
d_l = [__double2ll_rd, __double2ll_rn, __double2ll_ru, __double2ll_rz, __double_as_longlong, __ll2double_rd, __ll2double_rn, __ll2double_ru, __ll2double_rz, __longlong_as_double]

d_ul :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => [PartialStackOp w]
d_ul = [__double2ull_rd, __double2ull_rn, __double2ull_ru, __double2ull_rz, __ull2double_rd, __ull2double_rn, __ull2double_ru, __ull2double_rz]

-- | Exported Ops

-- | Convert a double to a float in round-down mode.
__double2float_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w D) => PartialStackOp w
__double2float_rd = opify (Op "__double2float_rd" :: Op (OneArg D) F)

-- | Convert a double to a float in round-to-nearest-even mode.
__double2float_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w D) => PartialStackOp w
__double2float_rn = opify (Op "__double2float_rn" :: Op (OneArg D) F)

-- | Convert a double to a float in round-up mode.
__double2float_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w D) => PartialStackOp w
__double2float_ru = opify (Op "__double2float_ru" :: Op (OneArg D) F)

-- | Convert a double to a float in round-towards-zero mode.
__double2float_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w D) => PartialStackOp w
__double2float_rz = opify (Op "__double2float_rz" :: Op (OneArg D) F)

-- | Reinterpret high 32 bits in a double as a signed integer.
__double2hiint :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => PartialStackOp w
__double2hiint = opify (Op "__double2hiint" :: Op (OneArg D) I)

-- | Convert a double to a signed int in round-down mode.
__double2int_rd :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => PartialStackOp w
__double2int_rd = opify (Op "__double2int_rd" :: Op (OneArg D) I)

-- | Convert a double to a signed int in round-to-nearest-even mode.
__double2int_rn :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => PartialStackOp w
__double2int_rn = opify (Op "__double2int_rn" :: Op (OneArg D) I)

-- | Convert a double to a signed int in round-up mode.
__double2int_ru :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => PartialStackOp w
__double2int_ru = opify (Op "__double2int_ru" :: Op (OneArg D) I)

-- | Convert a double to a signed int in round-towards-zero mode.
__double2int_rz :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => PartialStackOp w
__double2int_rz = opify (Op "__double2int_rz" :: Op (OneArg D) I)

-- | Convert a double to a signed 64-bit int in round-down mode.
__double2ll_rd :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__double2ll_rd = opify (Op "__double2ll_rd" :: Op (OneArg D) L)

-- | Convert a double to a signed 64-bit int in round-to-nearest-even mode.
__double2ll_rn :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__double2ll_rn = opify (Op "__double2ll_rn" :: Op (OneArg D) L)

-- | Convert a double to a signed 64-bit int in round-up mode.
__double2ll_ru :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__double2ll_ru = opify (Op "__double2ll_ru" :: Op (OneArg D) L)

-- | Convert a double to a signed 64-bit int in round-towards-zero mode.
__double2ll_rz :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__double2ll_rz = opify (Op "__double2ll_rz" :: Op (OneArg D) L)

-- | Reinterpret low 32 bits in a double as a signed integer.
__double2loint :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => PartialStackOp w
__double2loint = opify (Op "__double2loint" :: Op (OneArg D) I)

-- | Convert a double to an unsigned int in round-down mode.
__double2uint_rd :: (HasIdentifier w, HasStackLens w D, HasStackLens w UI) => PartialStackOp w
__double2uint_rd = opify (Op "__double2uint_rd" :: Op (OneArg D) UI)

-- | Convert a double to an unsigned int in round-to-nearest-even mode.
__double2uint_rn :: (HasIdentifier w, HasStackLens w D, HasStackLens w UI) => PartialStackOp w
__double2uint_rn = opify (Op "__double2uint_rn" :: Op (OneArg D) UI)

-- | Convert a double to an unsigned int in round-up mode.
__double2uint_ru :: (HasIdentifier w, HasStackLens w D, HasStackLens w UI) => PartialStackOp w
__double2uint_ru = opify (Op "__double2uint_ru" :: Op (OneArg D) UI)

-- | Convert a double to an unsigned int in round-towards-zero mode.
__double2uint_rz :: (HasIdentifier w, HasStackLens w D, HasStackLens w UI) => PartialStackOp w
__double2uint_rz = opify (Op "__double2uint_rz" :: Op (OneArg D) UI)

-- | Convert a double to an unsigned 64-bit int in round-down mode.
__double2ull_rd :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => PartialStackOp w
__double2ull_rd = opify (Op "__double2ull_rd" :: Op (OneArg D) UL)

-- | Convert a double to an unsigned 64-bit int in round-to-nearest-even mode.
__double2ull_rn :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => PartialStackOp w
__double2ull_rn = opify (Op "__double2ull_rn" :: Op (OneArg D) UL)

-- | Convert a double to an unsigned 64-bit int in round-up mode.
__double2ull_ru :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => PartialStackOp w
__double2ull_ru = opify (Op "__double2ull_ru" :: Op (OneArg D) UL)

-- | Convert a double to an unsigned 64-bit int in round-towards-zero mode.
__double2ull_rz :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => PartialStackOp w
__double2ull_rz = opify (Op "__double2ull_rz" :: Op (OneArg D) UL)

-- | Reinterpret bits in a double as a 64-bit signed integer.
__double_as_longlong :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__double_as_longlong = opify (Op "__double_as_longlong" :: Op (OneArg D) L)

-- | Convert a float to a signed integer in round-down mode.
__float2int_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__float2int_rd = opify (Op "__float2int_rd" :: Op (OneArg F) I)

-- | Convert a float to a signed integer in round-to-nearest-even mode.
__float2int_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__float2int_rn = opify (Op "__float2int_rn" :: Op (OneArg F) I)

-- | Convert a float to a signed integer in round-up mode.
__float2int_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__float2int_ru = opify (Op "__float2int_ru" :: Op (OneArg F) I)

-- | Convert a float to a signed integer in round-towards-zero mode.
__float2int_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__float2int_rz = opify (Op "__float2int_rz" :: Op (OneArg F) I)

-- | Convert a float to a signed 64-bit integer in round-down mode.
__float2ll_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
__float2ll_rd = opify (Op "__float2ll_rd" :: Op (OneArg F) L)

-- | Convert a float to a signed 64-bit integer in round-to-nearest-even mode.
__float2ll_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
__float2ll_rn = opify (Op "__float2ll_rn" :: Op (OneArg F) L)

-- | Convert a float to a signed 64-bit integer in round-up mode.
__float2ll_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
__float2ll_ru = opify (Op "__float2ll_ru" :: Op (OneArg F) L)

-- | Convert a float to a signed 64-bit integer in round-towards-zero mode.
__float2ll_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
__float2ll_rz = opify (Op "__float2ll_rz" :: Op (OneArg F) L)

-- | Convert a float to an unsigned integer in round-down mode.
__float2uint_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__float2uint_rd = opify (Op "__float2uint_rd" :: Op (OneArg F) UI)

-- | Convert a float to an unsigned integer in round-to-nearest-even mode.
__float2uint_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__float2uint_rn = opify (Op "__float2uint_rn" :: Op (OneArg F) UI)

-- | Convert a float to an unsigned integer in round-up mode.
__float2uint_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__float2uint_ru = opify (Op "__float2uint_ru" :: Op (OneArg F) UI)

-- | Convert a float to an unsigned integer in round-towards-zero mode.
__float2uint_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__float2uint_rz = opify (Op "__float2uint_rz" :: Op (OneArg F) UI)

-- | Convert a float to an unsigned 64-bit integer in round-down mode.
__float2ull_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => PartialStackOp w
__float2ull_rd = opify (Op "__float2ull_rd" :: Op (OneArg F) UL)

-- | Convert a float to an unsigned 64-bit integer in round-to-nearest-even mode.
__float2ull_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => PartialStackOp w
__float2ull_rn = opify (Op "__float2ull_rn" :: Op (OneArg F) UL)

-- | Convert a float to an unsigned 64-bit integer in round-up mode.
__float2ull_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => PartialStackOp w
__float2ull_ru = opify (Op "__float2ull_ru" :: Op (OneArg F) UL)

-- | Convert a float to an unsigned 64-bit integer in round-towards-zero mode.
__float2ull_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => PartialStackOp w
__float2ull_rz = opify (Op "__float2ull_rz" :: Op (OneArg F) UL)

-- | Reinterpret bits in a float as a signed integer.
__float_as_int :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__float_as_int = opify (Op "__float_as_int" :: Op (OneArg F) I)

-- | Reinterpret bits in a float as a unsigned integer.
__float_as_uint :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__float_as_uint = opify (Op "__float_as_uint" :: Op (OneArg F) UI)

-- | Reinterpret high and low 32-bit integer values as a double.
__hiloint2double :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => PartialStackOp w
__hiloint2double = opify (Op "__hiloint2double" :: Op (TwoArg I I) D)

-- | Convert a signed int to a double.
__int2double_rn :: (HasIdentifier w, HasStackLens w D, HasStackLens w I) => PartialStackOp w
__int2double_rn = opify (Op "__int2double_rn" :: Op (OneArg I) D)

-- | Convert a signed integer to a float in round-down mode.
__int2float_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__int2float_rd = opify (Op "__int2float_rd" :: Op (OneArg I) F)

-- | Convert a signed integer to a float in round-to-nearest-even mode.
__int2float_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__int2float_rn = opify (Op "__int2float_rn" :: Op (OneArg I) F)

-- | Convert a signed integer to a float in round-up mode.
__int2float_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__int2float_ru = opify (Op "__int2float_ru" :: Op (OneArg I) F)

-- | Convert a signed integer to a float in round-towards-zero mode.
__int2float_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__int2float_rz = opify (Op "__int2float_rz" :: Op (OneArg I) F)

-- | Reinterpret bits in an integer as a float.
__int_as_float :: (HasIdentifier w, HasStackLens w F, HasStackLens w I) => PartialStackOp w
__int_as_float = opify (Op "__int_as_float" :: Op (OneArg I) F)

-- | Convert a signed 64-bit int to a double in round-down mode.
__ll2double_rd :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__ll2double_rd = opify (Op "__ll2double_rd" :: Op (OneArg L) D)

-- | Convert a signed 64-bit int to a double in round-to-nearest-even mode.
__ll2double_rn :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__ll2double_rn = opify (Op "__ll2double_rn" :: Op (OneArg L) D)

-- | Convert a signed 64-bit int to a double in round-up mode.
__ll2double_ru :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__ll2double_ru = opify (Op "__ll2double_ru" :: Op (OneArg L) D)

-- | Convert a signed 64-bit int to a double in round-towards-zero mode.
__ll2double_rz :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__ll2double_rz = opify (Op "__ll2double_rz" :: Op (OneArg L) D)

-- | Convert a signed integer to a float in round-down mode.
__ll2float_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
__ll2float_rd = opify (Op "__ll2float_rd" :: Op (OneArg L) F)

-- | Convert a signed 64-bit integer to a float in round-to-nearest-even mode.
__ll2float_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
__ll2float_rn = opify (Op "__ll2float_rn" :: Op (OneArg L) F)

-- | Convert a signed integer to a float in round-up mode.
__ll2float_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
__ll2float_ru = opify (Op "__ll2float_ru" :: Op (OneArg L) F)

-- | Convert a signed integer to a float in round-towards-zero mode.
__ll2float_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w L) => PartialStackOp w
__ll2float_rz = opify (Op "__ll2float_rz" :: Op (OneArg L) F)

-- | Reinterpret bits in a 64-bit signed integer as a double.
__longlong_as_double :: (HasIdentifier w, HasStackLens w D, HasStackLens w L) => PartialStackOp w
__longlong_as_double = opify (Op "__longlong_as_double" :: Op (OneArg L) D)

-- | Convert an unsigned int to a double.
__uint2double_rn :: (HasIdentifier w, HasStackLens w D, HasStackLens w UI) => PartialStackOp w
__uint2double_rn = opify (Op "__uint2double_rn" :: Op (OneArg UI) D)

-- | Convert an unsigned integer to a float in round-down mode.
__uint2float_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__uint2float_rd = opify (Op "__uint2float_rd" :: Op (OneArg UI) F)

-- | Convert an unsigned integer to a float in round-to-nearest-even mode.
__uint2float_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__uint2float_rn = opify (Op "__uint2float_rn" :: Op (OneArg UI) F)

-- | Convert an unsigned integer to a float in round-up mode.
__uint2float_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__uint2float_ru = opify (Op "__uint2float_ru" :: Op (OneArg UI) F)

-- | Convert an unsigned integer to a float in round-towards-zero mode.
__uint2float_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__uint2float_rz = opify (Op "__uint2float_rz" :: Op (OneArg UI) F)

-- | Reinterpret bits in an unsigned integer as a float.
__uint_as_float :: (HasIdentifier w, HasStackLens w F, HasStackLens w UI) => PartialStackOp w
__uint_as_float = opify (Op "__uint_as_float" :: Op (OneArg UI) F)

-- | Convert an unsigned 64-bit int to a double in round-down mode.
__ull2double_rd :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => PartialStackOp w
__ull2double_rd = opify (Op "__ull2double_rd" :: Op (OneArg UL) D)

-- | Convert an unsigned 64-bit int to a double in round-to-nearest-even mode.
__ull2double_rn :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => PartialStackOp w
__ull2double_rn = opify (Op "__ull2double_rn" :: Op (OneArg UL) D)

-- | Convert an unsigned 64-bit int to a double in round-up mode.
__ull2double_ru :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => PartialStackOp w
__ull2double_ru = opify (Op "__ull2double_ru" :: Op (OneArg UL) D)

-- | Convert an unsigned 64-bit int to a double in round-towards-zero mode.
__ull2double_rz :: (HasIdentifier w, HasStackLens w D, HasStackLens w UL) => PartialStackOp w
__ull2double_rz = opify (Op "__ull2double_rz" :: Op (OneArg UL) D)

-- | Convert an unsigned integer to a float in round-down mode.
__ull2float_rd :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => PartialStackOp w
__ull2float_rd = opify (Op "__ull2float_rd" :: Op (OneArg UL) F)

-- | Convert an unsigned integer to a float in round-to-nearest-even mode.
__ull2float_rn :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => PartialStackOp w
__ull2float_rn = opify (Op "__ull2float_rn" :: Op (OneArg UL) F)

-- | Convert an unsigned integer to a float in round-up mode.
__ull2float_ru :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => PartialStackOp w
__ull2float_ru = opify (Op "__ull2float_ru" :: Op (OneArg UL) F)

-- | Convert an unsigned integer to a float in round-towards-zero mode.
__ull2float_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w UL) => PartialStackOp w
__ull2float_rz = opify (Op "__ull2float_rz" :: Op (OneArg UL) F)

