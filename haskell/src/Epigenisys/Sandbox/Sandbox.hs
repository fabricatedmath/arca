{-# LANGUAGE EmptyDataDeriving #-}
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
--import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable

import Epigenisys.Language.Stack

class HasIdentifier w where
    getIdentifier :: State w Int

data AST o where
    Literal :: o -> AST o
    UnaryExpression :: (Show o1, Typeable o1) => 
        { astOpId :: Int
        , astFuncName :: Text
        , astOperand :: AST o1
        } -> AST o
    BinaryExpression :: (Show o1, Typeable o1, Show o2, Typeable o2) =>
        { astOpId :: Int
        , astIsInfix :: Bool
        , astFuncName :: Text
        , astLeftOp :: AST o1
        , astRightOp :: AST o2
        } -> AST o
    TrinaryExpression :: (Show o1, Typeable o1, Show o2, Typeable o2, Show o3, Typeable o3) => 
        { astOpId :: Int
        , astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        } -> AST o
    QuadrinaryExpression :: (Show o1, Typeable o1, Show o2, Typeable o2, Show o3, Typeable o3, Show o4, Typeable o4) => 
        { astOpId :: Int
        , astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        , astOp4 :: AST o4
        } -> AST o

instance (Show o, Typeable o) => Show (AST o) where
    show (Literal o) = 
        "Literal(" <> show o <> " :: " ++ show (typeOf o) ++ ")"

    show e@(UnaryExpression _ident name o) = 
        T.unpack name <> "( " <> show o <> " )" <> " :: " <> show (typeRep e)

    show e@(BinaryExpression _ident _isInfix name o1 o2) = 
        T.unpack name <> "( " <> show o1 <> ", " <> show o2 <> " )" <> " :: " <> show (typeRep e)

    show e@(TrinaryExpression _ident name o1 o2 o3) = 
        T.unpack name <> "( " <> show o1 <> ", " <> show o2 <> ", " <> show o3 <> " )" <> " :: " <> show (typeRep e)

    show e@(QuadrinaryExpression _ident name o1 o2 o3 o4) = 
        T.unpack name <> "( " <> show o1 <> ", " <> show o2 <> ", " <> show o3 <> ", " <> show o4 <> " )" <> " :: " <> show (typeRep e)

data OneArg a = OneArg a
data TwoArg a b = TwoArg a b
data ThreeArg a b c = ThreeArg a b c
data FourArg a b c d = FourArg a b c d

data C_UnsignedInt deriving Show
data C_Int deriving Show

data C_LongLongInt deriving Show
data C_UnsignedLongLongInt deriving Show

data C_Float deriving Show
data C_Double deriving Show

data C_LongInt deriving Show

type D = AST C_Double
type F = AST C_Float

type I = AST C_Int
type UI = AST C_UnsignedInt
type L = AST C_LongLongInt
type UL = AST C_UnsignedLongLongInt
type LI = AST C_LongInt

data Pointer a = Pointer a

{-
__fmad :: (HasIdentifier w, HasStackLens w F) => State w ()
__fmad = opify op 
    where op = prefixOp "__fmad" :: Op (TwoArg F F) (OneArg F)
    
__double2float_rz :: (HasIdentifier w, HasStackLens w F, HasStackLens w D) => State w ()
__double2float_rz = opify op 
    where op = prefixOp "__double2float_rz" :: Op (OneArg D) (OneArg F)

-}

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

instance (Show o1, Typeable o1) => ToExpression (OneArg (AST o1)) where
    toExpression i _ t (OneArg a) = UnaryExpression i t a

instance (Show o1, Typeable o1, Show o2, Typeable o2) => ToExpression (TwoArg (AST o1) (AST o2)) where
    toExpression i isInfix t (TwoArg a b) = BinaryExpression i isInfix t a b

instance (Show o1, Typeable o1, Show o2, Typeable o2, Show o3, Typeable o3) => ToExpression (ThreeArg (AST o1) (AST o2) (AST o3)) where
    toExpression i _isInfix t (ThreeArg a b c) = TrinaryExpression i t a b c

instance (Show o1, Typeable o1, Show o2, Typeable o2, Show o3, Typeable o3, Show o4, Typeable o4) => ToExpression (FourArg (AST o1) (AST o2) (AST o3) (AST o4)) where
    toExpression i _isInfix t (FourArg a b c d) = QuadrinaryExpression i t a b c d

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

instance forall w a b c. (HasStackLens w a, HasStackLens w b, HasStackLens w c) => OpIn w (ThreeArg a b c) where
    opin _ = 
        do
            w <- get
            ma <- popL stackLens :: State w (Maybe a)
            mb <- popL stackLens :: State w (Maybe b)
            mc <- popL stackLens :: State w (Maybe c)
            let m = ThreeArg <$> ma <*> mb <*> mc
            when (isNothing m) $ put w
            pure m

instance forall w a b c d. (HasStackLens w a, HasStackLens w b, HasStackLens w c, HasStackLens w d) => OpIn w (FourArg a b c d) where
    opin _ = 
        do
            w <- get
            ma <- popL stackLens :: State w (Maybe a)
            mb <- popL stackLens :: State w (Maybe b)
            mc <- popL stackLens :: State w (Maybe c)
            md <- popL stackLens :: State w (Maybe d)
            let m = FourArg <$> ma <*> mb <*> mc <*> md
            when (isNothing m) $ put w
            pure m