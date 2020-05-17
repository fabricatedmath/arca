{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Epigenisys.Worlds.CudaWorld where

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Read as T (decimal, signed)
import Data.Typeable

import GHC.Generics

import TextShow
import TextShow.Generic

import Epigenisys.Language
import Epigenisys.Language.Parser
import Epigenisys.Language.Ops
import Epigenisys.Language.Stack
import Epigenisys.Language.Types

class ConvertNum a b where
  convertNum :: a -> b

instance ConvertNum Int Int where convertNum = id
instance ConvertNum Double Double where convertNum = id
instance ConvertNum Int Double where convertNum = fromIntegral
instance ConvertNum Double Int where convertNum = round

data BinaryOperator = Addition | Subtraction | Multiplication -- | Exponentiation
    deriving (Generic, Show)
    deriving TextShow via FromGeneric BinaryOperator

data UnaryOperator = Abs | Signum | Negate | Convert
    deriving (Generic, Show)
    deriving TextShow via FromGeneric UnaryOperator

applyUnaryOp :: Typeable a => UnaryOperator -> proxy a -> Text -> Text
applyUnaryOp n u v = 
    case n of
        Convert -> "(" `T.append` T.toLower (showt $ typeRep u) `T.append` ")" `T.append` v
        Abs -> "abs(" `T.append` v `T.append` ")"
        Negate -> "(-" `T.append` v `T.append` ")"
        Signum -> "signum(" `T.append` v `T.append` ")"

applyBinaryOp :: BinaryOperator -> Text -> Text -> Text
applyBinaryOp n v1 v2 = 
    case n of 
        Addition -> v1 `T.append` " + " `T.append` v2
        Subtraction -> v1 `T.append` " - " `T.append` v2
        Multiplication -> v1 `T.append` " * " `T.append` v2

data AST o where
    Literal :: o -> AST o
    Variable :: Text -> AST o
    BinaryExpression 
        ::  ( ConvertNum l o, ConvertNum r o
            , Show l, Show r
            , TextShow l, TextShow r
            , Typeable l, Typeable r
            ) => BinaryOperator -> AST l -> AST r -> AST o
    UnaryExpression 
        ::  ( ConvertNum l o
            , Show l
            , TextShow l
            , Typeable l
            ) => UnaryOperator -> AST l -> AST o
    
instance (Show o, Typeable o) => Show (AST o) where
    show (Literal o) = "Literal(" ++ show o ++ " :: " ++ show (typeOf o) ++ ")"
    show (Variable s) = "Variable(" ++ T.unpack s ++ ")"
    show (BinaryExpression n l r) = show n ++ "(" ++ show l ++ ", " ++ show r ++ ")"
    show (UnaryExpression n o) = show n ++ "(" ++ show o ++ ")"

instance (TextShow o, Typeable o) => TextShow (AST o) where
    showb (Literal o) = fromText "Literal(" <> fromText (showt o) <> fromText " :: " <> fromText (showt (typeOf o)) <> fromText ")"
    showb (Variable s) = fromText "Variable(" <> fromText s <> fromText ")"
    showb (BinaryExpression n l r) = fromText (showt n) <> fromText "(" <> fromText (showt l) <> fromText ", " <> fromText (showt r) <> fromText ")"
    showb (UnaryExpression n o) = fromText (showt n) <> fromText "(" <> fromText (showt o) <> fromText ")"

instance ConvertNum (AST Int) (AST Double) where
    convertNum o = UnaryExpression Convert o

instance ConvertNum (AST Double) (AST Int) where
    convertNum o = UnaryExpression Convert o

instance (ConvertNum a a, Num a, Show a, TextShow a, Typeable a) => Num (AST a) where
    (+) = BinaryExpression Addition
    (-) = BinaryExpression Subtraction
    (*) = BinaryExpression Multiplication
    negate = UnaryExpression Negate
    abs = UnaryExpression Abs
    signum = UnaryExpression Signum 
    fromInteger = Literal . fromInteger

compile :: (TextShow o, Typeable o) => AST o -> Text
compile = T.toStrict . toLazyText . (\(_,_,c) -> c) . compile' 0
    where
        compile' :: (TextShow o, Typeable o) => Int -> AST o -> (Int, Text, Builder)
        compile' i (Literal o) = (i+1,var, b)
            where var = "a" `T.append` showt i
                  b = fromText (T.toLower (showt $ typeOf o)) <> fromText " " <> fromText var <> fromText " = " <> showb o <> fromText ";\n"
                
        compile' i (Variable s) = (i, s, fromText "")
        compile' i e@(UnaryExpression n l) = (i'+1, var, b <> b')
            where 
                (i',v, b) = compile' i l
                var = "a" `T.append` showt i'
                b' =  fromText (T.toLower (showt $ typeRep e)) <> fromText " " <> fromText var <> fromText " = " <> fromText (applyUnaryOp n e v) <> fromText ";\n"
        compile' i e@(BinaryExpression n l r) = (i''+1, var, b <> b' <> b'')
            where
                (i',vl, b) = compile' i l
                (i'',vr, b') = compile' i' r
                var = "a" `T.append` showt i''
                b'' = fromText (T.toLower (showt $ typeRep e)) <> fromText " " <> fromText var <> fromText " = " <> fromText (applyBinaryOp n vl vr) <> fromText ";\n"

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
        draw (Literal o) = lines $ "Literal(" ++ show o ++ ") :: " ++ show (typeOf o)
        draw v@(Variable s) = lines $ "Variable(" ++ show s ++ ") :: " ++ show (typeRep v)
        draw u@(UnaryExpression n o) = lines ("UnaryExpression " ++ show n ++ " :: (" ++ show (typeRep o) ++ " -> " ++ show (typeRep u) ++ ")") ++ drawSubTrees [o]
        draw b@(BinaryExpression n l r) = lines ("BinaryExpression " ++ show n ++ " :: (" ++ show (typeRep l) ++ " -> " ++ show (typeRep b) ++ ")" ++ " (" ++ show (typeRep r) ++ " -> " ++ show (typeRep b) ++ ")") ++ drawSubTrees [l] ++ drawSubTrees [r]

drawAST :: (TextShow o, Typeable o) => AST o -> Text
drawAST = T.toStrict . toLazyText . unlinesB . draw
    where
        drawSubTrees [] = []
        drawSubTrees [t] = 
            "|" : shift "`- " "   " (draw t)
        drawSubTrees (t:ts) =
            "|" : shift "+- " "|  " (draw t) <> drawSubTrees ts

        shift first other = zipWith (<>) (first : repeat other)

        draw :: (TextShow o, Typeable o) => AST o -> [Builder]
        draw (Literal o) = pure $ fromText "Literal(" <> showb o <> fromText ") :: " <> showb (typeOf o)
        draw v@(Variable s) = pure $ fromText "Variable(" <> showb s <> fromText ") :: " <> showb (typeRep v)
        draw u@(UnaryExpression n o) = pure (fromText "UnaryExpression " <> showb n <> fromText " :: (" <> showb (typeRep o) <> fromText " -> " <> showb (typeRep u) <> fromText ")") <> drawSubTrees [o]
        draw b@(BinaryExpression n l r) = pure (fromText "BinaryExpression " <> showb n <> fromText " :: (" <> showb (typeRep l) <> fromText " -> " <> showb (typeRep b) <> fromText ")" <> fromText " (" <> showb (typeRep r) <> fromText " -> " <> showb (typeRep b) <> fromText ")") <> drawSubTrees [l] ++ drawSubTrees [r]


data World = 
    World 
    { _execStack :: Stack (Exec World)
    , _intStack :: Stack (AST Int)
    } 
    deriving (Generic, Show)
    deriving TextShow via FromGeneric World

makeLenses ''World

instance HasEmpty World where
    getEmpty = 
        World 
        { _execStack = empty
        , _intStack = empty
        }

instance HasStackLens World (Exec World) where
    stackLens = execStack

instance HasStackLens World (AST Int) where
    stackLens = intStack

instance HasStack World (Exec World) where
    stackName _ = StackName "Exec"
    stackOps _ = []
    stackParseLiteral _ = Nothing

instance HasStack World (AST Int) where
    stackName _ = StackName "Integer"
    stackOps _ = 
        [ addOp intProxy
        , multiplyOp intProxy
        ] where intProxy = Proxy :: Proxy (AST Int)
    stackParseLiteral _ = Just f
        where f t = either (const $ pure $ Variable t) (pure . Literal) $ textRead (T.signed T.decimal) t

instance HasWorldParser World where
    worldTypes = 
        [ StackType (Proxy :: Proxy (World, Exec World))
        , StackType (Proxy :: Proxy (World, AST Int))
        ]
