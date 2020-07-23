{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except

import Data.Text (Text)

import TextShow

import Arca.Language 
import Arca.Worlds.SimpleWorld

testProgram :: Text
testProgram = "((Integer.1 Integer.+) (Integer.4) Integer.*)"

testProgram2 :: Text
testProgram2 = "(Integer.dogs (Integer.2 Integer.+) Integer.4 Integer.*)"

main :: IO ()
main = 
  do
    void $ runExceptT $ do
      liftIO $ print testProgram
      w <- liftEither $ parseLang testProgram :: ExceptT String IO (LanguageTree (StackOp World))
      liftIO $ print w
      liftIO $ putStrLn $ drawLanguageTree $ fmap show w

    printT $ (runLang 100 testProgram :: Either String World)
