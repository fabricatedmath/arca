{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except

import Data.Text (Text)

import TextShow

import Epigenisys.Language 
import Epigenisys.Worlds.SimpleWorld

testProgram :: Text
testProgram = "(Integer.1 Integer.2 Integer.+ Integer.4 Integer.*)"

testProgram2 :: Text
testProgram2 = "(Integer.dogs Integer.2 Integer.+ Integer.4 Integer.*)"

main :: IO ()
main = 
  do
--    printT $ (runLang testProgram :: Either String SymbolWorld)
    printT $ (runLang testProgram :: Either String World)
--    printT $ (runLang testProgram2 :: Either String C.World)
{-
    void $ runExceptT $ do
      w <- liftEither $ (runLang testProgram2 :: Either String C.World) :: ExceptT String IO (C.World)
      liftIO $ mapM_ (T.putStrLn . C.drawAST) $ unStack (C._intStack w)
      liftIO $ mapM_ (T.putStrLn . C.compile) $ unStack (C._intStack w)
-}