{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.Internal.VectorTypes where

import TextShow

import Arca.Cuda.World.Internal.AST

newtype C_Float2 = C_Float2 ()
    deriving Show

instance TextShow C_Float2 where
    showb (C_Float2 ui) = showb ui

instance C_Type C_Float2 where
    ctypep _ = "float2"
    cval (C_Float2 a) = showt a

newtype C_Float4 = C_Float4 ()
    deriving Show

instance TextShow C_Float4 where
    showb (C_Float4 ui) = showb ui

instance C_Type C_Float4 where
    ctypep _ = "float4"
    cval (C_Float4 a) = showt a

newtype C_Int2 = C_Int2 ()
    deriving Show

instance TextShow C_Int2 where
    showb (C_Int2 ui) = showb ui

instance C_Type C_Int2 where
    ctypep _ = "int2"
    cval (C_Int2 a) = showt a

newtype C_Int4 = C_Int4 ()
    deriving Show

instance TextShow C_Int4 where
    showb (C_Int4 ui) = showb ui

instance C_Type C_Int4 where
    ctypep _ = "int4"
    cval (C_Int4 a) = showt a

type F2 = AST C_Float2
type F4 = AST C_Float4
type I2 = AST C_Int2
type I4 = AST C_Int4
