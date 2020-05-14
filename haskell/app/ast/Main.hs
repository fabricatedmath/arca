{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict

import Epigenisys.Language (drawLanguageTree, WorldParserMap, parseText, worldParser, opify, printWorldStackOps)
import Epigenisys.SymbolWorld

import Data.Proxy
import Data.Text (Text)

testProgram :: Text
testProgram = "(Integer.1 Integer.2 Integer.+ Integer.4 Integer.*)"

main :: IO ()
main = 
  do
    let eWorld = do 
                  textTree <- parseText testProgram
                  let parserMap = worldParser :: WorldParserMap SymbolWorld
                  opTree <- opify parserMap textTree
                  let world = startSymbolWorld $ Exec opTree
                  return $ execState runSymbolWorld world
    print $ eWorld