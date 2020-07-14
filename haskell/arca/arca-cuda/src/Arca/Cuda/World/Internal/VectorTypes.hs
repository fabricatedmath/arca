{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.Internal.VectorTypes where

import TextShow

import Arca.Cuda.World.Internal.AST

newtype C_Float4 = C_Float4 ()
    deriving Show

instance TextShow C_Float4 where
    showb (C_Float4 ui) = showb ui

instance C_Type C_Float4 where
    ctypep _ = "float4"
    cval (C_Float4 a) = showt a

-- | can pop float4, push .x, push float4 again, pop .y, etc...
-- | this invalidates OpOut TwoArg, ThreeArg, FourArg