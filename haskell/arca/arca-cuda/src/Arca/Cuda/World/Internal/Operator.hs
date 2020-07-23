{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arca.Cuda.World.Internal.Operator
    ( opify
    , Op(..)
    , OneArg, TwoArg, TwoArgInfix, ThreeArg, FourArg
    , VArg1, VArg2, VArg3, VArg4
    ) where

import Data.Text (Text)

import Arca.Cuda.World.Internal.AST
import Arca.Language

data Op i o = Op { opName :: Text }

data OneArg a = OneArg a
data TwoArg a b = TwoArg a b
data TwoArgInfix a b = TwoArgInfix a b
data ThreeArg a b c = ThreeArg a b c
data FourArg a b c d = FourArg a b c d

type VArg1 a = OneArg a
type VArg2 a = TwoArg a a
type VArg3 a = ThreeArg a a a
type VArg4 a = FourArg a a a a

{- Opify -}

opify :: forall w i o. (OpIn w i, OpOut w i o) => Op i o -> PartialStackOp w
opify (Op text) = PartialStackOp (OpName text) $
    do
        i <- opin (Proxy :: Proxy i)
        opout (Proxy :: Proxy o) text i

{- ToExpression -}

class ToExpression a where
    toExpression :: UniqueId -> Text -> a -> AST o

instance 
    ( HasAST o1 
    ) => ToExpression (OneArg (AST o1)) where
    toExpression uniqueId t (OneArg a) = UnaryExpression uniqueId t a

instance 
    ( HasAST o1, HasAST o2 
    ) => ToExpression (TwoArg (AST o1) (AST o2)) where
    toExpression uniqueId t (TwoArg a b) = BinaryExpression uniqueId t a b

instance
    ( HasAST o1, HasAST o2
    ) => ToExpression (TwoArgInfix (AST o1) (AST o2)) where
    toExpression uniqueId t (TwoArgInfix a b) = BinaryExpressionInfix uniqueId t a b

instance
    ( HasAST o1, HasAST o2, HasAST o3
    ) => ToExpression (ThreeArg (AST o1) (AST o2) (AST o3)) where
    toExpression uniqueId t (ThreeArg a b c) = TrinaryExpression uniqueId t a b c

instance 
    ( HasAST o1, HasAST o2, HasAST o3, HasAST o4
    ) => ToExpression (FourArg (AST o1) (AST o2) (AST o3) (AST o4)) where
    toExpression uniqueId t (FourArg a b c d) = QuadrinaryExpression uniqueId t a b c d

{- OpIn -}

class OpIn w a where
    opin:: Proxy a -> StateT w Maybe a

instance
    ( HasStackLens w a 
    ) => OpIn w (OneArg a) where
    opin Proxy = curryState arg 
        where arg = OneArg :: a -> OneArg a

instance
    ( HasStackLens w a
    , HasStackLens w b
    ) => OpIn w (TwoArg a b) where
    opin Proxy = curryState arg 
        where arg = TwoArg :: a -> b -> TwoArg a b

instance
    ( HasStackLens w a
    , HasStackLens w b
    ) => OpIn w (TwoArgInfix a b) where
    opin Proxy = curryState arg 
        where arg = TwoArgInfix :: a -> b -> TwoArgInfix a b

instance
    ( HasStackLens w a
    , HasStackLens w b
    , HasStackLens w c
    ) => OpIn w (ThreeArg a b c) where
    opin Proxy = curryState arg
        where arg = ThreeArg :: a -> b -> c -> ThreeArg a b c

instance
    ( HasStackLens w a
    , HasStackLens w b
    , HasStackLens w c
    , HasStackLens w d
    ) => OpIn w (FourArg a b c d) where
    opin Proxy = curryState arg
        where arg = FourArg :: a -> b -> c -> d -> FourArg a b c d

{- OpOut -}

class OpOut w i o where
    opout :: MonadState w m => Proxy o -> Text -> i -> m ()

instance forall w i o a. 
    ( ToExpression i, HasIdentifier w 
    , o ~ AST a, HasStackLens w o
    ) => OpOut w i o where
    opout Proxy t i = 
        do
            uniqueId <- ratchetId
            pushL (stackLens :: StackLens w o) $ toExpression uniqueId t i



