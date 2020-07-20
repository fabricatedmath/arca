{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Tree (Tree(..), drawTree)

{-
    Dual number implementation, dual numbers are complex-like numbers with the rule i * i = 0 instead of i * i = -1
    Dual numbers "imaginary" part maintains the derivative as it gets passed around, real part maintains the normal calculation
    Imaginary part is set to 1 at start
-}

data Dual a = Dual a a 
    deriving (Read, Show, Eq)

instance Num a => Num (Dual a) where
  (+) (Dual u u') (Dual v v') = Dual (u + v) (u' + v')
  (*) (Dual u u') (Dual v v') = Dual (u * v) (u' * v + u * v')
  (-) (Dual u u') (Dual v v') = Dual (u - v) (u' - v')
  negate (Dual u u')          = Dual (negate u) (negate u')
  abs (Dual u u')             = Dual (abs u) (u' * (signum u))
  signum (Dual u u')          = Dual (signum u) 0
  fromInteger n               = Dual (fromInteger n) 0

instance (Floating a, Fractional a) => Fractional (Dual a) where
  (/) (Dual u u') (Dual v v') = Dual (u / v) ((u' * v - u * v') / v ** 2)
  recip (Dual u u')           = Dual (recip u) (-1 * u' * (recip (u ** 2)))
  fromRational n              = Dual (fromRational n) 0

instance (Floating a) => Floating (Dual a) where
  pi                = Dual pi 0
  exp (Dual u u')   = Dual (exp u) (u' * exp u)
  sqrt (Dual u u')  = Dual (sqrt u) (u' / (2 * sqrt u))
  log (Dual u u')   = Dual (log u) (u' / u)
  sin (Dual u u')   = Dual (sin u) (u' * cos u)
  cos (Dual u u')   = Dual (cos u) (- u' * sin u)
  tan (Dual u u')   = Dual (tan u) (1 / ((cos u) ** 2))
  asin (Dual u u')  = Dual (asin u) (u' / (sqrt(1 - u ** 2)))
  acos (Dual u u')  = Dual (acos u) (- u' / (sqrt(1 - u ** 2)))
  atan (Dual u u')  = Dual (atan u) (u' / (1 + u ** 2))
  sinh (Dual u u')  = Dual (sinh u) (u' * cosh u)
  cosh (Dual u u')  = Dual (cosh u) (u' * sinh u)
  tanh (Dual u u')  = Dual (tanh u) (u' * (1 - (tanh u) ** 2))
  asinh (Dual u u') = Dual (asinh u) (u' / (sqrt(1 + u ** 2)))
  acosh (Dual u u') = Dual (acosh u) (u' / (sqrt(u ** 2 - 1)))
  atanh (Dual u u') = Dual (atanh u) (u' / (1 - u ** 2))
  (**) (Dual u u') (Dual v v')
    = Dual (u ** v) (u ** v * (v' * (log u) + (v * u' / u)))
  logBase (Dual u u') (Dual v v')
    = Dual (logBase u v) (((log v) * u' / u - (log u) * v' / v) / ((log u) ** 2))

{-
    AST to record what happens to numbers
-}

data AST = AST String | LiteralInteger Integer | Variable String | LiteralFloat Float
    deriving Show

mkVariable :: String -> Tree AST
mkVariable varName = Node (Variable varName) []

instance Num (Tree AST) where
    (+) a b = Node (AST "+") [a, b]
    (-) a b = Node (AST "-") [a, b]
    (*) a b = Node (AST "*") [a, b]
    negate a = Node (AST "negate") [a]
    abs a = Node (AST "abs") [a]
    signum a = Node (AST "signum") [a]
    fromInteger i = Node (LiteralInteger i) []

instance Fractional (Tree AST) where
    (/) a b = Node (AST "/") [a, b]
    recip a = Node (AST "recip") [a]
    fromRational r = Node (LiteralFloat $ fromRational r) []

instance Floating (Tree AST) where
    pi = Node (LiteralFloat pi) []
    exp a = Node (AST "exp") [a]
    log a = Node (AST "log") [a]
    sqrt a = Node (AST "sqrt") [a]
    (**) a b = Node (AST "**") [a, b]
    logBase a b = Node (AST "logBase") [a, b]
    sin a = Node (AST "sin") [a]
    cos a = Node (AST "cos") [a]
    tan a = Node (AST "tan") [a]
    asin a = Node (AST "asin") [a]
    acos a = Node (AST "acos") [a]
    atan a = Node (AST "atan") [a]
    sinh a = Node (AST "sinh") [a]
    cosh a = Node (AST "cosh") [a]
    tanh a = Node (AST "tanh") [a]
    asinh a = Node (AST "asinh") [a]
    acosh a = Node (AST "acosh") [a]
    atanh a = Node (AST "atanh") [a]

mkDualAST :: Dual (Tree AST)
mkDualAST = Dual (Node (Variable "x") []) (Node (LiteralFloat 1) [])

printDualTree :: Dual (Tree AST) -> IO ()
printDualTree (Dual realPart imagPart) = 
    do
        putStrLn "Real Part:\n"
        putStrLn $ drawTree $ show <$> realPart
        putStr "\n\n"
        putStrLn "Imag Part(Derivative):\n"
        putStrLn $ drawTree $ show <$> imagPart

-- sample functions, generic in Floating class (can pass Float, Double, (Tree AST), or even Dual (Tree AST))
f :: Floating a => a -> a
f x = cos (x^2)

g :: Floating a => a -> a
g x = x^2 + 3*x^3 + cos (x^2)

main :: IO ()
main = 
    do
        let x = 2 :: Float

        putStrLn "Running f.."
        putStr "\n"
        putStrLn $ "Answer at " <> show x <> " is " <> show (f x)
        putStr "\n"

        let dualAns = f mkDualAST
        printDualTree dualAns
    
        putStrLn "Running g.."
        putStr "\n"
        putStrLn $ "Answer at " <> show x <> " is " <> show (g x)
        putStr "\n"

        let dualAns = g mkDualAST
        printDualTree dualAns
