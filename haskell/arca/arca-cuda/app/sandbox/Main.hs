{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Proxy

import qualified Data.Text.IO as T

import Arca.Language

import Arca.Cuda.World
import Arca.Cuda.World.Internal
import Arca.Cuda.World.Ops
import Arca.Cuda.World.SpecialOps

main :: IO ()
main = 
    do
        let stackOp = psoToSo (Namespace "Test") testOp :: StackOp World
            tree = Exec $ Expression $ stackOp
        T.putStrLn $ printWorld $ runProg 100 tree

        print stackOp

        let proxy = Proxy :: Proxy C_Float
            stackOp2 = psoToSo (Namespace "Test") $ warpShuffle proxy :: StackOp World
            --varOp = psoToSo (Namespace "Test") $ variableOp proxy "var" :: StackOp World
            intOp = psoToSo (Namespace "Test") $ literalOp $ C_Int 2 :: StackOp World
            floatOp = psoToSo (Namespace "Test") $ literalOp $ C_Float 3 :: StackOp World
            tree2 = Exec $ Open $ map Expression [intOp, floatOp, stackOp2]
        T.putStrLn $ printWorld $ runProg 100 tree2
        --print tree

