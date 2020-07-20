{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Typeable 

import Numeric.AD

data BinaryOp = 
    Add | Subtract | Multiply | --Num
    Divide | --Fractional
    Exponent | LogBase --Floating
    deriving Show
data UnaryOp = 
    Negate | Abs | Signum | --Num
    Recip | --Fractional
    Exp | Log | Sqrt | --Floating
    Sin | Cos | Tan | 
    ASin | ACos | ATan | 
    SinH | CosH | TanH | 
    ASinH | ACosH | ATanH 
    deriving Show

data AST o where
    VariableExpression :: Text -> AST o
    LiteralExpression :: o -> AST o
    UnaryExpression :: (Show o1, Typeable o1) => UnaryOp -> AST o1 -> AST o
    BinaryExpression :: (Show o1, Typeable o1, Show o2, Typeable o2) => BinaryOp -> AST o1 -> AST o2 -> AST o

deriving instance Show o => Show (AST o)

instance (Num o, Show o, Typeable o) => Num (AST o) where
    (+) = BinaryExpression Add
    (-) = BinaryExpression Subtract
    (*) = BinaryExpression Multiply
    negate = UnaryExpression Negate
    abs = UnaryExpression Abs
    signum = UnaryExpression Signum
    fromInteger = LiteralExpression . fromInteger

instance (Fractional o, Show o, Typeable o) => Fractional (AST o) where
    (/) = BinaryExpression Divide
    recip = UnaryExpression Recip
    fromRational = LiteralExpression . fromRational

instance (Floating o, Show o, Typeable o) => Floating (AST o) where
    pi = LiteralExpression pi
    exp = UnaryExpression Exp
    log = UnaryExpression Log
    sqrt = UnaryExpression Sqrt
    (**) = BinaryExpression Exponent
    logBase = BinaryExpression LogBase
    sin = UnaryExpression Sin
    cos = UnaryExpression Cos
    tan = UnaryExpression Tan
    asin = UnaryExpression ASin
    acos = UnaryExpression ACos
    atan = UnaryExpression ATan
    sinh = UnaryExpression SinH
    cosh = UnaryExpression CosH
    tanh = UnaryExpression TanH
    asinh = UnaryExpression ASinH
    acosh = UnaryExpression ACosH
    atanh = UnaryExpression ATanH

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
        draw (LiteralExpression o) = 
            lines $ "Literal" <> " - " <> "(" <> show o <> ") :: " <> show (typeOf o)
        draw v@(VariableExpression s) = 
            lines $ "Variable(" <> show s <> ") :: " <> show (typeOf v)
        draw e@(UnaryExpression name o1) = 
            lines ("UnaryExpression" <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep e]
        draw e@(BinaryExpression name o1 o2) = 
            lines ("BinaryExpression" <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep e]

