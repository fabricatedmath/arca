{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Epigenisys.Worlds.CudaWorld where

import Control.Lens

import Data.Char (toLower)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Read as T (decimal, signed)
import Data.Typeable

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
    deriving Show

data UnaryOperator = Abs | Signum | Negate | Convert
    deriving Show

applyUnaryOp :: Typeable a => UnaryOperator -> proxy a -> String -> String
applyUnaryOp n u v = 
    case n of
        Convert -> "(" ++ map toLower (show $ typeRep u) ++ ")" ++ v
        Abs -> "abs(" ++ v ++ ")"
        Negate -> "-" ++ v
        Signum -> "signum(" ++ v ++ ")"

applyBinaryOp :: BinaryOperator -> String -> String -> String
applyBinaryOp n v1 v2 = 
    case n of 
        Addition -> v1 ++ " + " ++ v2
        Subtraction -> v1 ++ " - " ++ v2
        Multiplication -> v1 ++ " * " ++ v2

data AST o where
    Literal :: o -> AST o
    Variable :: String -> AST o
    BinaryExpression :: (ConvertNum l o, ConvertNum r o, Show l, Show r, Typeable l, Typeable r) 
        => BinaryOperator -> AST l -> AST r -> AST o
    UnaryExpression :: (ConvertNum l o, Show l, Typeable l) 
        => UnaryOperator -> AST l -> AST o
    
instance (Show o, Typeable o) => Show (AST o) where
    show (Literal o) = "Literal(" ++ show o ++ " :: " ++ show (typeOf o) ++ ")"
    show (Variable s) = "Variable(" ++ s ++ ")"
    show (BinaryExpression n l r) = show n ++ "(" ++ show l ++ ", " ++ show r ++ ")"
    show (UnaryExpression n o) = show n ++ "(" ++ show o ++ ")"

instance ConvertNum (AST Int) (AST Double) where
    convertNum o = UnaryExpression Convert o

instance ConvertNum (AST Double) (AST Int) where
    convertNum o = UnaryExpression Convert o

instance (ConvertNum a a, Num a, Show a, Typeable a) => Num (AST a) where
    (+) = BinaryExpression Addition
    (-) = BinaryExpression Subtraction
    (*) = BinaryExpression Multiplication
    negate = UnaryExpression Negate
    abs = UnaryExpression Abs
    signum = UnaryExpression Signum 
    fromInteger = Literal . fromInteger


-- convert this to use Text
compile :: (Show o, Typeable o) => AST o -> IO ()
compile = void . compile' 0
    where
        compile' :: (Show o, Typeable o) => Int -> AST o -> IO (Int,String)
        compile' i (Literal o) = 
            do
                let var = "a" ++ show i
                putStrLn $ map toLower (show $ typeOf o) ++ " " ++ var ++ " = " ++ show o ++ ";"
                return (i+1,var)
        compile' i (Variable s) = return (i,s)
        compile' i u@(UnaryExpression n l) = 
            do
                (i',v) <- compile' i l
                let var = "a" ++ show i'
                putStrLn $ map toLower (show $ typeRep u) ++ " " ++ var ++ " = " ++ applyUnaryOp n u v ++ ";"
                return (i'+1, var)
        compile' i b@(BinaryExpression n l r) = 
            do
                (i',vl) <- compile' i l
                (i'',vr) <- compile' i' r
                let var = "a" ++ show i''
                putStrLn $ map toLower (show $ typeRep b) ++ " " ++ var ++ " = " ++ applyBinaryOp n vl vr ++ ";"
                return (i''+1, var)

drawAST :: (Show o, Typeable o) => AST o -> String
drawAST = unlines . draw
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


data World = 
    World 
    { _execStack :: Stack (Exec World)
    , _intStack :: Stack (AST Int)
    } deriving Show

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
        where f t = either (\_ -> pure $ Variable $ T.unpack t) (pure . Literal) $ (textRead (T.signed T.decimal) t)

        {-
        Just $ fmap (\i -> Literal i) . ei
            where ei = textRead $ T.signed T.decimal
            -}

instance HasWorldParser World where
    worldTypes = 
        [ StackType (Proxy :: Proxy (World, Exec World))
        , StackType (Proxy :: Proxy (World, AST Int))
        ]
