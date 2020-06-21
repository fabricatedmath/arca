{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Epigenisys.Language.Parser (drawLanguageTree, WorldParserMap, parseText, worldParser, opify)
--import Epigenisys.Worlds.SimpleWorld
import Control.Monad (replicateM_)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Epigenisys.FFI.CUContextContainer
import Epigenisys.FFI.NvrtcContainer

import Epigenisys.Language.Mutate

import Epigenisys.Worlds.CudaWorld

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

tabLines' :: [Text] -> [Text]
tabLines' = map ("  " <>)

deviceFunc :: [Text] -> Text
deviceFunc behavior = 
    T.unlines $ 
    [ "__device__"
    , "float deviceFunc() {"
    ] <> 
    tabLines' behavior <> 
    [ "}"
    ]

globalFunc :: Text
globalFunc = 
    T.unlines $ 
    [ "extern \"C\" {"
    , "__global__"
    , "void kernel() {"
    , "  " <> "float x = deviceFunc();"
    , "  " <> "printf(\"%f\\n\",x);"
    , "}"
    , "}"
    ]

cudaFunc :: [Text] -> Text
cudaFunc behavior = 
    deviceFunc behavior <> "\n" <>
    globalFunc


randomFunc :: CUContextContainer -> IO ()
randomFunc cuCtx = 
    do
        T.putStrLn "\n\n\n"
        w <- generateRandomWorld
        case generateACudaProgram w of
            Nothing -> putStrLn "No program found"
            Just b -> 
                do
                    T.putStrLn $ printWorld w
                    let program = cudaFunc b
                    T.putStrLn $ "Compiling:\n\n" <> program 
                    
                    c_nvrtcContainerInit
                    nvrtcContainer <- newNvrtcContainer cuCtx
                    compiled <- compileNvrtcContainer nvrtcContainer program
                    if compiled then runNvrtcContainer nvrtcContainer 1 32 else putStrLn "Couldn't run"
main :: IO ()
main = 
    do
        --doStuff
        doOtherStuff
        runStuff
        cuCtx <- newCUContextContainer
        replicateM_ 10 $ randomFunc cuCtx

