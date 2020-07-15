{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arca.Cuda.World.Internal.Operator
    ( opify
    , Op(..)
    , OneArg
    , TwoArg
    , TwoArgInfix
    , ThreeArg
    , FourArg
    ) where

import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Typeable

import TextShow

import Arca.Cuda.World.Internal.AST

import Arca.Language

data Op i o = Op { opName :: Text }

data OneArg a = OneArg a
data TwoArg a b = TwoArg a b
data TwoArgInfix a b = TwoArgInfix a b
data ThreeArg a b c = ThreeArg a b c
data FourArg a b c d = FourArg a b c d

{- Opify -}

opify :: forall w i o. (OpIn w i, OpOut w i o) => Op i o -> PartialStackOp w
opify (Op text) = PartialStackOp (OpName text) $
    do
        mi <- opin (Proxy :: Proxy i)
        case mi of
            Nothing -> pure ()
            Just i -> opout (Proxy :: Proxy o) text i

{- ToExpression -}

class ToExpression a where
    toExpression :: UniqueId -> Text -> a -> AST o

instance
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    ) => ToExpression (OneArg (AST o1)) where
    toExpression i t (OneArg a) = UnaryExpression i t a

instance
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    , C_Type o2, Show o2, TextShow o2, Typeable o2
    ) => ToExpression (TwoArg (AST o1) (AST o2)) where
    toExpression i t (TwoArg a b) = BinaryExpression i t a b

instance
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    , C_Type o2, Show o2, TextShow o2, Typeable o2
    ) => ToExpression (TwoArgInfix (AST o1) (AST o2)) where
    toExpression i t (TwoArgInfix a b) = BinaryExpressionInfix i t a b

instance
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    , C_Type o2, Show o2, TextShow o2, Typeable o2
    , C_Type o3, Show o3, TextShow o3, Typeable o3
    ) => ToExpression (ThreeArg (AST o1) (AST o2) (AST o3)) where
    toExpression i t (ThreeArg a b c) = TrinaryExpression i t a b c

instance
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    , C_Type o2, Show o2, TextShow o2, Typeable o2
    , C_Type o3, Show o3, TextShow o3, Typeable o3
    , C_Type o4, Show o4, TextShow o4, Typeable o4
    ) => ToExpression (FourArg (AST o1) (AST o2) (AST o3) (AST o4)) where
    toExpression i t (FourArg a b c d) = QuadrinaryExpression i t a b c d

{- OpIn -}

class OpIn w a where
    opin:: Proxy a -> State w (Maybe a)

instance
    HasStackLens w a 
    => OpIn w (OneArg a) where
    opin Proxy = 
        do
            w <- get
            ma <- popL stackLens
            let m = OneArg <$> ma
            when (isNothing m) $ put w
            pure m

instance
    ( HasStackLens w a
    , HasStackLens w b
    ) => OpIn w (TwoArg a b) where
    opin Proxy = 
        do
            w <- get
            ma <- popL stackLens
            mb <- popL stackLens
            let m = TwoArg <$> ma <*> mb
            when (isNothing m) $ put w
            pure m

instance
    ( HasStackLens w a
    , HasStackLens w b
    ) => OpIn w (TwoArgInfix a b) where
    opin Proxy = 
        do
            w <- get
            ma <- popL stackLens
            mb <- popL stackLens
            let m = TwoArgInfix <$> ma <*> mb
            when (isNothing m) $ put w
            pure m

instance
    ( HasStackLens w a
    , HasStackLens w b
    , HasStackLens w c
    ) => OpIn w (ThreeArg a b c) where
    opin Proxy = 
        do
            w <- get
            ma <- popL stackLens
            mb <- popL stackLens
            mc <- popL stackLens
            let m = ThreeArg <$> ma <*> mb <*> mc
            when (isNothing m) $ put w
            pure m

instance
    ( HasStackLens w a
    , HasStackLens w b
    , HasStackLens w c
    , HasStackLens w d
    ) => OpIn w (FourArg a b c d) where
    opin Proxy = 
        do
            w <- get
            ma <- popL stackLens
            mb <- popL stackLens
            mc <- popL stackLens
            md <- popL stackLens
            let m = FourArg <$> ma <*> mb <*> mc <*> md
            when (isNothing m) $ put w
            pure m

{- OpOut -}

class OpOut w a b where
    opout :: Proxy b -> Text -> a -> State w ()

instance forall w i a o1. 
    ( ToExpression i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    ) => OpOut w i a where
    opout Proxy t a = 
        do
            i <- ratchetId
            pushL (stackLens :: StackLens w a) $ toExpression i t a
