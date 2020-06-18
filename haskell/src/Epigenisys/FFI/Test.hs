{-# LANGUAGE OverloadedStrings #-}

module Epigenisys.FFI.Test where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad (replicateM_,when)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Time

import Foreign.C.String

import Foreign.ForeignPtr
import Foreign.Ptr

import Foreign.Marshal.Alloc

import Epigenisys.FFI.CUContextContainer

data NvrtcContainerHandle

data NvrtcContainer =
  NvrtcContainer
  { _nvrtcContainer :: ForeignPtr NvrtcContainerHandle
  }

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
        getNumCapabilities >>= print
        let prog1 = (deviceFunc2 <> globalFunc2)
            prog2 = globalFunc
        c_nvrtcContainerInit
        start <- getCurrentTime
        nvrtcContainer <- newNvrtcContainer cuCtx
        b1 <- compileNvrtcContainer nvrtcContainer prog1
        nvrtcContainer2 <- newNvrtcContainer cuCtx
        b2 <- compileNvrtcContainer nvrtcContainer2 prog2
        end <- getCurrentTime
        print (diffUTCTime end start)

        chan <- newChan

        replicateM_ 100 $ forkOS $ 
            do
                start <- getCurrentTime
                threadId <- myThreadId
                --cuCtx2 <- newCUContextContainer
                threadCapability threadId >>= print
                --threadCapability 
                n <- getNumCapabilities
                putStrLn $ "num capabilities: " <> show n
                print "failed"
                c_nvrtcContainerInit
                print "running"
                nvrtcContainer <- newNvrtcContainer cuCtx
                print "running2"
                b3 <- compileNvrtcContainer nvrtcContainer prog1
                print "running3"
                writeChan chan ()
                end <- getCurrentTime
                print (diffUTCTime end start)
                if b3 then runNvrtcContainer nvrtcContainer 1 1024 else print "couldnt run 3"
                --print "ran"
        replicateM_ 100 (readChan chan)
        --if b1 then runNvrtcContainer nvrtcContainer 1 32 else print "couldnt run 1"
        --if b2 then runNvrtcContainer nvrtcContainer2 1 32 else print "couldnt run 2"


newNvrtcContainer :: CUContextContainer -> IO NvrtcContainer
newNvrtcContainer cuContextContainerHandle = 
    do
        withForeignPtr (_cuContextContainerHandle cuContextContainerHandle) (\ctxPtr -> 
            do
                nvrtcContainerHandle <- c_nvrtcContainerNew ctxPtr >>= newForeignPtr c_nvrtcContainerDelete
                return $ NvrtcContainer nvrtcContainerHandle
            )

compileNvrtcContainer :: NvrtcContainer -> Text -> IO Bool
compileNvrtcContainer container prog = 
        withForeignPtr (_nvrtcContainer container) (\containerPtr -> 
                T.withCStringLen prog $ uncurry (c_nvrtcContainerCompile containerPtr)
         )

runNvrtcContainer :: NvrtcContainer -> Int -> Int -> IO ()
runNvrtcContainer container numBlocks numThreads = 
        withForeignPtr (_nvrtcContainer container) (\containerPtr -> 
                c_nvrtcContainerRun containerPtr numBlocks numThreads
         )


foreign import ccall unsafe "nvrtcContainerNew" c_nvrtcContainerNew
    :: Ptr CUContextContainerHandle -> IO (Ptr NvrtcContainerHandle)

foreign import ccall unsafe "nvrtcContainerInit" c_nvrtcContainerInit
    :: IO ()

foreign import ccall unsafe "nvrtcContainerCompile" c_nvrtcContainerCompile
    :: Ptr NvrtcContainerHandle -> CString -> Int -> IO Bool

foreign import ccall unsafe "nvrtcContainerRun" c_nvrtcContainerRun
    :: Ptr NvrtcContainerHandle -> Int -> Int -> IO ()

foreign import ccall unsafe "&nvrtcContainerDelete" c_nvrtcContainerDelete
    :: FinalizerPtr NvrtcContainerHandle
