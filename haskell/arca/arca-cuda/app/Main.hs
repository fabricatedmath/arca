{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM_)
import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import TextShow

import Arca.Cuda.FFI.CUContextContainer
import Arca.Cuda.FFI.PtxCompiler
import Arca.Cuda.FFI.PtxLinker

import Arca.Language.Mutate

import Arca.Cuda.World

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

cudaFunc :: [Text] -> Text
cudaFunc behavior = deviceFunc behavior <> "\n" <> globalFunc
    where
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
            , "  " <> "if (threadIdx.x == 0) {"
            , "  " <> "printf(\"%f\\n\",x);"
            , "  " <> "}"
            , "}"
            , "}"
            ]

-- | Handle maybe case of generateACudaProgram
cudaFuncMultiSM :: [World] -> Text
cudaFuncMultiSM worlds = 
    let
        (deviceFuncs, globalContribs) = unzip $ zipWith block [0..] worlds
    in
        T.unlines $ 
        map T.unlines deviceFuncs <> 
        globalFuncHeader <>
        map (T.unlines . tabLines') globalContribs <> 
        tabLines' globalFuncPrint <>
        globalFuncFooter
    where
        deviceFunc :: Int -> World -> (Text, [Text])
        deviceFunc blockNum w = (funcName, funcDecl)
            where
                Just behavior = generateACudaProgram w
                funcName = "deviceFunc_" <> showt blockNum
                funcDecl =
                    [ "__device__"
                    , "float " <> funcName <> "()" <> " {"
                    ] <> 
                    tabLines' behavior <> 
                    [ "}"
                    ]
        globalFuncHeader :: [Text]
        globalFuncHeader = 
            [ "extern \"C\" {"
            , "__global__"
            , "void kernel() {"
            , "  float x;"
            ]
        
        globalFuncPrint :: [Text]
        globalFuncPrint = 
            [ "if (threadIdx.x == 0) {"
            , "  " <> "printf(\"fromBlock: %d, %f\\n\", blockIdx.x, x);"
            , "}"
            ]

        globalFuncFooter :: [Text]
        globalFuncFooter = 
            [ "}"
            , "}"
            ]

        block :: Int -> World -> ([Text], [Text])
        block i w = 
            let
                (funcName, funcDecl) = deviceFunc i w
                globalContribution = 
                    [ "if (blockIdx.x == " <> showt i <> ") {"
                    , "  " <> "x = " <> funcName <> "()" <> ";"
                    , "}"
                    ]
            in (funcDecl, globalContribution)        


manageExceptT :: ExceptT Text IO () -> IO ()
manageExceptT exceptT = 
    do
        e <- runExceptT exceptT
        case e of
            Left err -> T.putStrLn err
            Right () -> return ()

randomFunc ::  IO ()
randomFunc = 
    do
        T.putStrLn "\n\n\n"
        w <- generateRandomWorld
        case generateACudaProgram w of
            Nothing -> putStrLn "No program found"
            Just b -> 
                do
                    --T.putStrLn $ printWorld w
                    let program = cudaFunc b
                    T.putStrLn $ "Compiling:\n\n" <> program 
                    manageExceptT $ do
                        (linkLog,runner) <- ptxCompile program >>= ptxLink (FuncName "kernel")
                        liftIO $ do
                            T.putStrLn "Log:"
                            T.putStrLn linkLog
                            T.putStrLn "End Log"
                        ptxRun runner 1 32

randomFuncs :: IO () 
randomFuncs = 
    do
        let numBlocks = 32
        ws <- replicateM numBlocks generateRandomWorld
        let cudaCode = cudaFuncMultiSM ws 
        T.putStrLn cudaCode
        manageExceptT $ do
            ptxCode <- ptxCompile cudaCode
            (linkLog,runner) <- ptxLink (FuncName "kernel") ptxCode
            liftIO $ do
                T.putStrLn "Log:"
                T.putStrLn linkLog
                T.putStrLn "End Log"
            ptxRun runner numBlocks 32

main :: IO ()
main = 
    do
        ctxContainer <- newCUContextContainer
        --setCurrentContext ctxContainer
        randomFuncs
        --doStuff
        --doOtherStuff
        --runStuff

        --replicateM_ 10 $ randomFunc 

