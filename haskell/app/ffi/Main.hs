{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkOS, getNumCapabilities)
import Control.Concurrent.Chan
import Control.Monad (replicateM_)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Time

import Epigenisys.FFI.CUContextContainer
import Epigenisys.FFI.NvrtcContainer

globalFunc :: Text
globalFunc = 
    T.unlines 
    [ "extern \"C\" {"
    , "__global__"
    , "void kernel() {"
    , "  if (threadIdx.x == 0) {"
    , "    printf(\"dogs:%d\\n\", threadIdx.x);"
    , "  }"
    , "}"
    , "}"
    ]

deviceFunc2 :: Text
deviceFunc2 = 
    T.unlines
    [ "namespace {"
    , "__device__"
    , "void call() {"
    , "  if (threadIdx.x == 0) {"
    , "    printf(\"dogs:%d\\n\", threadIdx.x);"
    , "  }"
    , "}"    
    , "}"
    ]

globalFunc2 :: Text
globalFunc2 = 
    T.unlines 
    [ "extern \"C\" {"
    , "__global__"
    , "void kernel() {"
    , "  for (int i = 0; i < blockDim.x; i++) {"
    , "  if (threadIdx.x == i) {"
    , "    printf(\"%d\\n\", i);"
    , "  }"
    , "  }"
    , "  call();"
    , "}"
    , "}"
    ]

run :: IO ()
run = 
    do
        cuCtx <- newCUContextContainer
        start <- getCurrentTime
        getNumCapabilities >>= print
        let prog1 = (deviceFunc2 <> globalFunc2)
            prog2 = globalFunc
        c_nvrtcContainerInit
        nvrtcContainer0 <- newNvrtcContainer cuCtx
        b1 <- compileNvrtcContainer nvrtcContainer0 prog1
        nvrtcContainer1 <- newNvrtcContainer cuCtx
        b2 <- compileNvrtcContainer nvrtcContainer1 prog2
       
        --if b1 then runNvrtcContainer nvrtcContainer0 1 32 else putStrLn "couldnt run 1"
        --if b2 then runNvrtcContainer nvrtcContainer1 1 32 else putStrLn "couldnt run 2"

        chan <- newChan

        replicateM_ 10 $ forkOS $ 
            do
                --start2 <- getCurrentTime
                c_nvrtcContainerInit
                nvrtcContainer2 <- newNvrtcContainer cuCtx
                b3 <- compileNvrtcContainer nvrtcContainer2 prog2
                --end2 <- getCurrentTime
                --print (diffUTCTime end2 start2)
                --if b3 then runNvrtcContainer nvrtcContainer2 1 1024 else putStrLn "couldnt run 3"
                writeChan chan ()

        replicateM_ 10 (readChan chan)
        end <- getCurrentTime
        print (diffUTCTime end start)


main :: IO ()
main = run
        