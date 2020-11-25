{-# LANGUAGE OverloadedStrings #-}

module Main where

import Numeric.AD

import Lib

f :: Floating a => a -> a
f x = x^2 + 3*x^3 + cos (x^2)

main :: IO ()
main = do
    putStrLn $ drawASTString $ diff f $ (VariableExpression "x" :: AST Double)
    let (x,x') = diff' f $ (VariableExpression "x" :: AST Double)
    putStrLn $ drawASTString x
    putStrLn $ drawASTString x'
