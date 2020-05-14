{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.State.Strict

import Data.Char (toLower)

import Data.Foldable (fold, traverse_)
import Data.Monoid
import Data.Tree
import Data.Typeable

import System.Random

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

data AST o where
    Literal :: o -> AST o
    Variable :: String -> AST o
    BinaryExpression :: (ConvertNum l o, ConvertNum r o, Show l, Show r, Typeable l, Typeable r) 
        => BinaryOperator -> AST l -> AST r -> AST o
    UnaryExpression :: (ConvertNum l o, Show l, Typeable l) 
        => UnaryOperator -> AST l -> AST o
    
instance (Show o, Typeable o) => Show (AST o) where
    show (Literal o) = "Literal(" ++ show o ++ show (typeOf o) ++ ")"
    show (Variable s) = "Variable(" ++ s ++ ")"
    show (BinaryExpression n l r) = show n ++ "(" ++ show l ++ "," ++ show r ++ ")"
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

class Randomizable a where
    randomize :: a -> Int -> String

instance Randomizable Int where
    randomize a i = show a ++ ": " ++ show i

instance Randomizable String where
    randomize a i = show a ++ ": " ++ show i

instance Randomizable Char where
    randomize a i = show a ++ ": " ++ show i

data Op = forall a. (Show a, Randomizable a) => Op a

data Converted a b = Converted a b

data Floated a = Floated a

data Inted a = Inted a

-- | thread state through tree to produce symbols with traverse
-- | each converted generates a cuda cast statement to new symbol

instance Randomizable Op where
    randomize (Op a) i = randomize a i

deriving instance Show Op

--instance Show Op where
 --   show (Op a) = show a

-- | use dual monoid over tree traversal to flip args

count :: a -> State Int Int
count b = 
    do
        a <- get
        put (a+1)
        return a


stateFunc :: (RandomGen g, Randomizable a) => a -> State g String
stateFunc a = 
    do
        r <- state (randomR (0,10))
        return $ randomize a r


main :: IO () 
main = 
    do
        let tree = Node (Op (1 :: Int)) [Node (Op 'c') [], Node (Op "1") []]
        print tree
        g <- getStdGen
        print $ evalState (traverse stateFunc tree) g

        print $ evalState (traverse count tree) 0
        traverse_ print tree

        print $ fold $ fmap (Dual . show) tree

        print $ fold $ fmap Sum $ evalState (traverse count tree) 0
        