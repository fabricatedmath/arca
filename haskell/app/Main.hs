{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Epigenisys.Language.Parser (drawLanguageTree, WorldParserMap, parseText, worldParser, opify)
--import Epigenisys.Worlds.SimpleWorld
import Epigenisys.Worlds.CudaWorld
import Epigenisys.Language.Mutate

--import Data.Proxy
import Data.Text (Text)

testProgram1 :: Text
testProgram1 = "((Integer.+ Integer.1 Integer.2 (Integer.3 Float.2 Float.3.5 Float.+) Integer.*))"

testProgram2 :: Text
testProgram2 = "((Integer.3) Integer.+)"

testProgram3 :: Text
testProgram3 = "(INTEGER.1 Integer.+ INTEGER.2 Integer.- )"

testProgram4 :: Text
testProgram4 = "(Integer.1 Integer.2 Integer.+ Integer.convertToFloat2)"

main :: IO ()
main = 
    do
        --doStuff
        doOtherStuff
        runStuff

