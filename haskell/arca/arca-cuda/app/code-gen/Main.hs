module Main where

import Arca.Language.Mutate

import Arca.Cuda.World

main :: IO ()
main = 
    do
        doStuff 100
