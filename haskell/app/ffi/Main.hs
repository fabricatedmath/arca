{-# LANGUAGE OverloadedStrings #-}

module Main where

import Epigenisys.FFI.Test

main :: IO ()
main = 
    do
        print "dogs"
        testFunc "cats"