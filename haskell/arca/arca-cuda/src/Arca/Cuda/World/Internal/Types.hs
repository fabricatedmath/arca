{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.Internal.Types where

import Data.Hashable
import Data.Int
import Data.Word

import GHC.Generics 

import TextShow

import Arca.Cuda.World.Internal.AST

newtype C_Unsigned = C_Unsigned Word32
    deriving (Eq, Generic, Num, Ord, Show)

instance Hashable C_Unsigned

instance TextShow C_Unsigned where
    showb (C_Unsigned u) = showb u

instance C_Type C_Unsigned where
    ctypep _ = "unsigned"
    cval (C_Unsigned a) = showt a

newtype C_UnsignedInt = C_UnsignedInt Word32
    deriving (Eq, Generic, Num, Ord, Show)

instance Hashable C_UnsignedInt

instance TextShow C_UnsignedInt where
    showb (C_UnsignedInt ui) = showb ui

instance C_Type C_UnsignedInt where
    ctypep _ = "unsigned int"
    cval (C_UnsignedInt a) = showt a

newtype C_Int = C_Int Int32
    deriving (Eq, Generic, Num, Ord, Show)

instance Hashable C_Int

instance TextShow C_Int where
    showb (C_Int i) = showb i

instance C_Type C_Int where
    ctypep _ = "int"
    cval (C_Int a) = showt a

newtype C_LongInt = C_LongInt Int64
    deriving (Eq, Generic, Num, Ord, Show)

instance Hashable C_LongInt

instance TextShow C_LongInt where
    showb (C_LongInt li) = showb li

instance C_Type C_LongInt where
    ctypep _ = "long int"
    cval (C_LongInt a) = showt a

newtype C_LongLongInt = C_LongLongInt Int64
    deriving (Eq, Generic, Num, Ord, Show)

instance Hashable C_LongLongInt

instance TextShow C_LongLongInt where
    showb (C_LongLongInt lli) = showb lli

instance C_Type C_LongLongInt where
    ctypep _ = "long long int"
    cval (C_LongLongInt a) = showt a

newtype C_UnsignedLongLongInt = C_UnsignedLongLongInt Word64
    deriving (Eq, Generic, Num, Ord, Show)

instance Hashable C_UnsignedLongLongInt

instance TextShow C_UnsignedLongLongInt where
    showb (C_UnsignedLongLongInt ulli) = showb ulli

instance C_Type C_UnsignedLongLongInt where
    ctypep _ = "unsigned long long int"
    cval (C_UnsignedLongLongInt a) = showt a

newtype C_Float = C_Float Float
    deriving (Eq, Fractional, Generic, Num, Ord, Show)

instance Hashable C_Float

instance TextShow C_Float where
    showb (C_Float f) = showb f

instance C_Type C_Float where
    ctypep _ = "float"
    cval (C_Float a) = showt a

newtype C_Double = C_Double Double
    deriving (Eq, Fractional, Generic, Num, Ord, Show)
    
instance Hashable C_Double

instance TextShow C_Double where
    showb (C_Double d) = showb d

instance C_Type C_Double where
    ctypep _ = "double"
    cval (C_Double a) = showt a

type D = AST C_Double
type F = AST C_Float

type I = AST C_Int
type U = AST C_Unsigned
type UI = AST C_UnsignedInt
type L = AST C_LongLongInt
type UL = AST C_UnsignedLongLongInt
type LI = AST C_LongInt
