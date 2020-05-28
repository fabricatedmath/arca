{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}

module Epigenisys.Sandbox.Sandbox where

import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))
import Data.Text (Text)

import Epigenisys.Language.Stack

class HasIdentifier w where
    getIdentifier :: State w Int

data AST o where
    Literal :: o -> AST o
    UnaryExpression :: 
        { astOpId :: Int
        , astFuncName :: Text
        , astOperand :: AST b
        } ->  AST o
    BinaryExpression :: -- HasConstraint a => 
        { astOpId :: Int
        , astIsInfix :: Bool
        , astFuncName :: Text
        , astLeftOp :: AST l
        , astRightOp :: AST r
        } -> AST o

data OneArg a = OneArg a
data TwoArg a b = TwoArg a b
data ThreeArg a b = ThreeArg a b

data C_UnsignedInt
data C_Int

data C_LongLongInt
data C_UnsignedLongLongInt

data C_Float
data C_Double

type D = AST C_Double
type F = AST C_Float

type I = AST C_Int
type UI = AST C_UnsignedInt
type L = AST C_LongLongInt
type UL = AST C_UnsignedLongLongInt

data Pointer a = Pointer a

__fmad :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmad = opify op 
    where op = prefixOp "__fmad" :: Op (TwoArg F F) (OneArg F)
    
__double2float_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w D) => State w ()
__double2float_rz = opify op 
    where op = prefixOp "__double2float_rz" :: Op (OneArg D) (OneArg F)

data Op i o = 
    Op 
    { opIsInfix :: Bool
    , opName :: Text
    }

infixOp :: Text -> Op i o
infixOp = Op True

prefixOp :: Text -> Op i o
prefixOp = Op False

{- ToExpression -}

class ToExpression a where
    toExpression :: Int -> Bool -> Text -> a -> AST o

instance ToExpression (OneArg (AST a)) where
    toExpression i _ t (OneArg a) = UnaryExpression i t a

instance ToExpression (TwoArg (AST a) (AST b)) where
    toExpression i isInfix t (TwoArg a b) = BinaryExpression i isInfix t a b

{- Opify -}

class Opify w a where
    opify :: a -> State w ()

instance (OpIn w i, OpOut w i o) => Opify w (Op i o) where
    opify (Op isInfix text) = 
        do
            mi <- opin (Proxy :: Proxy i)
            case mi of
                Nothing -> pure ()
                Just i -> opout isInfix text (Proxy :: Proxy o) i

{- OpOut -}

class OpOut w a b where
    opout :: Bool -> Text -> Proxy b -> a -> State w ()

instance forall w i a o1. (a ~ AST o1, ToExpression i, HasStackLens w a, HasIdentifier w) => OpOut w i (OneArg a) where
    opout isInfix t _ a = 
        do
            i <- getIdentifier
            pushL (stackLens :: StackLens w a) $ toExpression i isInfix t a

instance forall w i a b o1 o2. (a ~ AST o1, b ~ AST o2, ToExpression i, HasStackLens w a, HasStackLens w b, HasIdentifier w) => OpOut w i (TwoArg a b) where
    opout isInfix t _ a = 
        do
            i <- getIdentifier
            pushL (stackLens :: StackLens w a) $ toExpression i isInfix t a
            pushL (stackLens :: StackLens w b) $ toExpression i isInfix t a

{- OpIn -}

class OpIn w a where
    opin:: Proxy a -> State w (Maybe a)

instance forall w a. HasStackLens w a => OpIn w (OneArg a) where
    opin _ = 
        do
            w <- get
            ma <- popL stackLens :: State w (Maybe a)
            let m = OneArg <$> ma
            when (isNothing m) $ put w
            pure m

instance forall w a b. (HasStackLens w a, HasStackLens w b) => OpIn w (TwoArg a b) where
    opin _ = 
        do
            w <- get
            ma <- popL stackLens :: State w (Maybe a)
            mb <- popL stackLens :: State w (Maybe b)
            let m = TwoArg <$> ma <*> mb
            when (isNothing m) $ put w
            pure m

