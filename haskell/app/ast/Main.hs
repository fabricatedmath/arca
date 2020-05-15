{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict
import Control.Monad.Except

import Epigenisys.FFI.Test
import Epigenisys.Language 
import Epigenisys.Language.Parser (parseLang, drawLanguageTree, WorldParserMap, parseText, worldParser, opify, printWorldStackOps)
import Epigenisys.Language.Types
import Epigenisys.Worlds.SimpleWorld
import Epigenisys.Worlds.SymbolWorld
import qualified Epigenisys.Worlds.CudaWorld as C

import Data.Proxy
import Data.Text (Text)

testProgram :: Text
testProgram = "(Integer.1 Integer.2 Integer.+ Integer.4 Integer.*)"

testProgram2 :: Text
testProgram2 = "(Integer.dogs Integer.2 Integer.+ Integer.4 Integer.*)"

main :: IO ()
main = 
  do
    print $ (runLang testProgram :: Either String SymbolWorld)
    print $ (runLang testProgram :: Either String World)
    print $ (runLang testProgram2 :: Either String C.World)
    void $ runExceptT $ do
      w <- liftEither $ (runLang testProgram2 :: Either String C.World) :: ExceptT String IO (C.World)
      liftIO $ mapM_ (putStrLn . C.drawAST) $ unStack (C._intStack w)
      liftIO $ mapM_ (C.compile) $ unStack (C._intStack w)
    return ()
    
      

