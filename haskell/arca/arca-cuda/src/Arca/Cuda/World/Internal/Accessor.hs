{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arca.Cuda.World.Internal.Accessor 
    ( accify
    , Acc(..)
    , OneField(..)
    , TwoField(..)
    , ThreeField(..)
    , FourField(..)
    ) where

import Data.Maybe (isNothing)

import Data.Text (Text)
import Data.Typeable

import TextShow

import Arca.Cuda.World.Internal.AST

import Arca.Language

data Acc i o = Acc Text o

data OneField a = OneField Text
data TwoField a b = TwoField Text Text
data ThreeField a b c = ThreeField Text Text Text
data FourField a b c d = FourField Text Text Text Text

{- Accify -}

accify :: forall w i o. (HasStackLens w i, OpOut w i o) => Acc i o -> PartialStackOp w
accify (Acc text o) = PartialStackOp (OpName text) $ 
    do
        mi <- opin (Proxy :: Proxy i)
        case mi of
            Nothing -> pure ()
            Just i -> opout i o

{- ToAccessor -}

class ToAccessor w a where
    toAccessor :: Text -> a -> State w (AST o)

instance 
    (C_Type o1, Show o1, TextShow o1, Typeable o1
    , HasIdentifier w
    ) => ToAccessor w (AST o1) where
    toAccessor t i = 
        do
            uniqueId <- ratchetId
            pure $ Accessor uniqueId t i

{- OpIn -}

opin :: HasStackLens w a => Proxy a -> State w (Maybe a)
opin Proxy = 
    do
        w <- get
        ma <- popL stackLens
        let m = ma
        when (isNothing m) $ put w
        pure m

{- OpOut -}

class OpOut w i o where
    opout :: i -> o -> State w ()

instance forall w i a o1. 
    ( ToAccessor w i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    ) => OpOut w i (OneField a) where
    opout i (OneField t1) = 
        do
            toAccessor t1 i >>= pushL (stackLens :: StackLens w a)

instance forall w i a o1 b o2. 
    ( ToAccessor w i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    , b ~ AST o2, HasStackLens w b
    ) => OpOut w i (TwoField a b) where
    opout i (TwoField t1 t2) = 
        do
            toAccessor t1 i >>= pushL (stackLens :: StackLens w a)
            toAccessor t2 i >>= pushL (stackLens :: StackLens w b)

instance forall w i a o1 b o2 c o3. 
    ( ToAccessor w i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    , b ~ AST o2, HasStackLens w b
    , c ~ AST o3, HasStackLens w c
    ) => OpOut w i (ThreeField a b c) where
    opout i (ThreeField t1 t2 t3) = 
        do
            toAccessor t1 i >>= pushL (stackLens :: StackLens w a)
            toAccessor t2 i >>= pushL (stackLens :: StackLens w b)
            toAccessor t3 i >>= pushL (stackLens :: StackLens w c)

instance forall w i a o1 b o2 c o3 d o4.
    ( ToAccessor w i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    , b ~ AST o2, HasStackLens w b
    , c ~ AST o3, HasStackLens w c
    , d ~ AST o4, HasStackLens w d
    ) => OpOut w i (FourField a b c d) where
    opout i (FourField t1 t2 t3 t4) = 
        do
            toAccessor t1 i >>= pushL (stackLens :: StackLens w a)
            toAccessor t2 i >>= pushL (stackLens :: StackLens w b)
            toAccessor t3 i >>= pushL (stackLens :: StackLens w c)
            toAccessor t4 i >>= pushL (stackLens :: StackLens w d)