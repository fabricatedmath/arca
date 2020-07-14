{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arca.Cuda.World.Internal.AST 
    ( AST(..), compile, drawASTString
    , C_Type(..), HasIdentifier(..)
    , ToExpression, OpIn, OpOut
    , Opify(..), Op(..)
    , OneArg, TwoArg, ThreeArg, FourArg
    , ratchetId
    ) where

import Control.Lens
import Control.Monad.RWS.Strict

import Data.List (intersperse, intercalate)
import Data.Maybe (isNothing)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import Data.Typeable

import TextShow

import Arca.Language

class HasIdentifier w where
    idLens :: Lens' w Int

ratchetId :: HasIdentifier w => State w Int
ratchetId = idLens %%= (\i -> (i,i+1))

data AST o where
    Call :: Int -> Text -> AST o -- | add var name to compile?
    Literal :: Int -> o -> AST o
    Variable :: Text -> AST o
    UnaryExpression :: 
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        ) => 
        { astOpId :: Int
        , astFuncName :: Text
        , astOperand :: AST o1
        } -> AST o
    BinaryExpression :: -- | split infix and prefix into separates?
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        , C_Type o2, Show o2, TextShow o2, Typeable o2
        ) =>
        { astOpId :: Int
        , astIsInfix :: Bool
        , astFuncName :: Text
        , astLeftOp :: AST o1
        , astRightOp :: AST o2
        } -> AST o
    TrinaryExpression :: 
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        , C_Type o2, Show o2, TextShow o2, Typeable o2
        , C_Type o3, Show o3, TextShow o3, Typeable o3
        ) => 
        { astOpId :: Int
        , astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        } -> AST o
    QuadrinaryExpression :: 
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        , C_Type o2, Show o2, TextShow o2, Typeable o2
        , C_Type o3, Show o3, TextShow o3, Typeable o3
        , C_Type o4, Show o4, TextShow o4, Typeable o4
        ) => 
        { astOpId :: Int
        , astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        , astOp4 :: AST o4
        } -> AST o

instance (Show o, Typeable o) => Show (AST o) where
    show c@(Call _ident call) = 
        "Call(" <> show call <> " :: " <> show (typeRep c) <> ")"
    show (Literal _ident o) = 
        "Literal(" <> show o <> " :: " <> show (typeOf o) <> ")"
    show v@(Variable s) = 
       "Variable(" <> T.unpack s <> " :: " <> show (typeRep v) <> ")"
    show e@(UnaryExpression _ident name o1) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1]
    show e@(BinaryExpression _ident _isInfix name o1 o2) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " $ [show o1, show o2]
    show e@(TrinaryExpression _ident name o1 o2 o3) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1, show o2, show o3]
    show e@(QuadrinaryExpression _ident name o1 o2 o3 o4) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1, show o2, show o3, show o4]

instance (TextShow o, Typeable o) => TextShow (AST o) where
    showb c@(Call _ident call) = 
        "Call(" <> T.fromText call <> " :: " <> showb (typeRep c) <> ")"
    showb (Literal _ident o) = 
        "Literal(" <> showb o <> " :: " <> showb (typeOf o) <> ")"
    showb v@(Variable s) = 
       "Variable(" <> T.fromText s <> " :: " <> showb (typeRep v) <> ")"
    showb e@(UnaryExpression _ident name o1) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1]
    showb e@(BinaryExpression _ident _isInfix name o1 o2) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " $ [showb o1, showb o2]
    showb e@(TrinaryExpression _ident name o1 o2 o3) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1, showb o2, showb o3]
    showb e@(QuadrinaryExpression _ident name o1 o2 o3 o4) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1, showb o2, showb o3, showb o4]

-- | Represents AST as multiline tabbed strings expressions
drawASTString :: (Show o, Typeable o) => AST o -> String
drawASTString = unlines . draw
    where
        drawSubTrees [] = []
        drawSubTrees [t] = 
            "|" : shift "`- " "   " (draw t)
        drawSubTrees (t:ts) =
            "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)

        draw :: (Show o, Typeable o) => AST o -> [String]
        draw c@(Call n call) = 
            lines $ "Call" <> " - " <> show n <> " - " <> "(" <> show call <> ") :: " <> show (typeOf c)
        draw (Literal n o) = 
            lines $ "Literal" <> " - " <> show n <> " - " <> "(" <> show o <> ") :: " <> show (typeOf o)
        draw v@(Variable s) = 
            lines $ "Variable(" <> show s <> ") :: " <> show (typeOf v)
        draw e@(UnaryExpression n name o1) = 
            lines ("UnaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep e]
        draw e@(BinaryExpression n _isInfix name o1 o2) = 
            lines ("BinaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep e]
        draw e@(TrinaryExpression n name o1 o2 o3) = 
            lines ("TrinaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2] <> drawSubTrees [o3]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep o3, typeRep e]
        draw e@(QuadrinaryExpression n name o1 o2 o3 o4) = 
            lines ("QuadrinaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2] <> drawSubTrees [o3] <> drawSubTrees [o4]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep o3, typeRep o4, typeRep e]

type CompileMonad a = RWS () [Text] IntSet a

-- | Compile AST into c-style statements (compatible with cuda)
compile :: (C_Type o, TextShow o, Typeable o) => AST o -> (Text, [Text])
compile ast = 
    let 
        (a,s) = evalRWS (compile' ast) () S.empty
        codeLines = map (`T.append` ";") $ s
    in (a, codeLines)
    where 
        gvn :: Int -> Text
        gvn n = "a" <> showt n

        checkAndAdd :: MonadState IntSet m => Int -> m Bool
        checkAndAdd i =
            do
                s <- get
                put $ S.insert i s
                pure $ S.member i s

        compile' :: (C_Type o, TextShow o, Typeable o) => AST o -> CompileMonad Text
        compile' c@(Call i call) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ tell $ pure $ ctypep c <> " " <> var <> " = " <> call
                pure var
        compile' (Literal i o) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ tell $ pure $ ctype o <> " " <> var <> " = " <> cval o
                pure var
        compile' (Variable s) = pure s
        compile' e@(UnaryExpression i name o) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar <- compile' o
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = ovar
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(BinaryExpression i isInfix name o1 o2) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs | isInfix = ovar1 <> " " <> name <> " " <> ovar2
                                | otherwise = name <> "(" <> args <> ")"
                                    where args = T.intercalate ", " [ovar1, ovar2]
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(TrinaryExpression i name o1 o2 o3) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        ovar3 <- compile' o3
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = T.intercalate ", " [ovar1, ovar2, ovar3]
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(QuadrinaryExpression i name o1 o2 o3 o4) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        ovar3 <- compile' o3
                        ovar4 <- compile' o4
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = T.intercalate ", " [ovar1, ovar2, ovar3, ovar4]
                        tell $ pure $ lhs <> rhs
                pure var

-- | split in and out for infix and accessor field

data OneArg a = OneArg a
data TwoArg a b = TwoArg a b
data ThreeArg a b c = ThreeArg a b c
data FourArg a b c d = FourArg a b c d

{-
data OneArg a = OneArg a
data TwoArg a b = TwoArg a b
data ThreeArg a b c = ThreeArg a b c
data FourArg a b c d = FourArg a b c d
-}


-- | add constructor for output args to carry accessor fields for multi arg returns
data Op i o = 
    Op 
    { opName :: Text
    }

ctype :: C_Type a => a -> Text
ctype = ctypep . Just

class (Show a, TextShow a, Typeable a) => C_Type a where
    -- | Name of c type in c language, e.g. "unsigned long long int"
    ctypep :: proxy a -> Text
    -- | Text representation of value, e.g. "395" or "2"
    cval :: a -> Text

{- ToExpression -}

class ToExpression a where
    toExpression :: Int -> Bool -> Text -> a -> AST o

instance 
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    ) => ToExpression (OneArg (AST o1)) where
    toExpression i _ t (OneArg a) = UnaryExpression i t a

instance 
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    , C_Type o2, Show o2, TextShow o2, Typeable o2
    ) => ToExpression (TwoArg (AST o1) (AST o2)) where
    toExpression i isInfix t (TwoArg a b) = BinaryExpression i isInfix t a b

-- | Tag TwoArg with infix, can get rid of isInifx

instance 
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    , C_Type o2, Show o2, TextShow o2, Typeable o2
    , C_Type o3, Show o3, TextShow o3, Typeable o3
    ) => ToExpression (ThreeArg (AST o1) (AST o2) (AST o3)) where
    toExpression i _isInfix t (ThreeArg a b c) = TrinaryExpression i t a b c

instance 
    ( C_Type o1, Show o1, TextShow o1, Typeable o1
    , C_Type o2, Show o2, TextShow o2, Typeable o2
    , C_Type o3, Show o3, TextShow o3, Typeable o3
    , C_Type o4, Show o4, TextShow o4, Typeable o4
    ) => ToExpression (FourArg (AST o1) (AST o2) (AST o3) (AST o4)) where
    toExpression i _isInfix t (FourArg a b c d) = QuadrinaryExpression i t a b c d

{- Opify -}

class Opify w a where
    opify :: a -> PartialStackOp w

instance (OpIn w i, OpOut w i o) => Opify w (Op i o) where
    opify (Op isInfix text) = PartialStackOp (OpName text) $
        do
            mi <- opin (Proxy :: Proxy i)
            case mi of
                Nothing -> pure ()
                Just i -> opout isInfix text (Proxy :: Proxy o) i

{- OpOut -}

class OpOut w a b where
    opout :: Bool -> Text -> Proxy b -> a -> State w ()

instance forall w i a o1. 
    ( ToExpression i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    ) => OpOut w i (OneArg a) where
    opout isInfix t _ a = 
        do
            i <- ratchetId
            pushL (stackLens :: StackLens w a) $ toExpression i isInfix t a

instance forall w i a o1 b o2. 
    ( ToExpression i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    , b ~ AST o2, HasStackLens w b
    ) => OpOut w i (TwoArg a b) where
    opout isInfix t _ a = 
        do
            i <- ratchetId
            pushL (stackLens :: StackLens w a) $ toExpression i isInfix t a
            pushL (stackLens :: StackLens w b) $ toExpression i isInfix t a

instance forall w i a o1 b o2 c o3. 
    ( ToExpression i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    , b ~ AST o2, HasStackLens w b
    , c ~ AST o3, HasStackLens w c
    ) => OpOut w i (ThreeArg a b c) where
    opout isInfix t _ a = 
        do
            i <- ratchetId
            pushL (stackLens :: StackLens w a) $ toExpression i isInfix t a
            pushL (stackLens :: StackLens w b) $ toExpression i isInfix t a
            pushL (stackLens :: StackLens w c) $ toExpression i isInfix t a

instance forall w i a o1 b o2 c o3 d o4. 
    ( ToExpression i, HasIdentifier w 
    , a ~ AST o1, HasStackLens w a
    , b ~ AST o2, HasStackLens w b
    , c ~ AST o3, HasStackLens w c
    , d ~ AST o4, HasStackLens w d
    ) => OpOut w i (FourArg a b c d) where
    opout isInfix t _ a = 
        do
            i <- ratchetId
            pushL (stackLens :: StackLens w a) $ toExpression i isInfix t a
            pushL (stackLens :: StackLens w b) $ toExpression i isInfix t a
            pushL (stackLens :: StackLens w c) $ toExpression i isInfix t a
            pushL (stackLens :: StackLens w d) $ toExpression i isInfix t a

{- OpIn -}

class OpIn w a where
    opin:: Proxy a -> State w (Maybe a)

instance
    HasStackLens w a 
    => OpIn w (OneArg a) where
    opin _ = 
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
    opin _ = 
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
    , HasStackLens w c
    ) => OpIn w (ThreeArg a b c) where
    opin _ = 
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
    opin _ = 
        do
            w <- get
            ma <- popL stackLens
            mb <- popL stackLens
            mc <- popL stackLens
            md <- popL stackLens
            let m = FourArg <$> ma <*> mb <*> mc <*> md
            when (isNothing m) $ put w
            pure m