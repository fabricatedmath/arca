{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict

import Epigenisys.FFI.Test
import Epigenisys.Language 
import Epigenisys.Language.Parser (parseLang, drawLanguageTree, WorldParserMap, parseText, worldParser, opify, printWorldStackOps)
import Epigenisys.Worlds.SimpleWorld
import Epigenisys.Worlds.SymbolWorld

import Data.Proxy
import Data.Text (Text)

testProgram :: Text
testProgram = "(Integer.1 Integer.2 Integer.+ Integer.4 Integer.*)"

main :: IO ()
main = 
  do
    print $ (runLang testProgram :: Either String SymbolWorld)
    print $ (runLang testProgram :: Either String World)
