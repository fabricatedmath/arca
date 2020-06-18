{-# LANGUAGE OverloadedStrings #-}

module Epigenisys.FFI.Test where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T

import Foreign.C.String

import Foreign.ForeignPtr
import Foreign.Ptr

import Foreign.Marshal.Alloc

data NvrtcContainerHandle

data NvrtcContainer =
  NvrtcContainer
  { _nvrtcContainerFPtr :: ForeignPtr NvrtcContainerHandle
  }

globalFunc :: Text
globalFunc = 
    T.unlines 
    [ "__global__"
    , "void kernel() {"
    , "  if (threadIdx.x == 0) {"
    , "    printf(\"%d\\n\", threadIdx.x);"
    , "  }"
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
    , "  if (threadIdx.x == 0) {"
    , "    printf(\"%d\\n\", threadIdx.x);"
    , "  }"
    , "  call();"
    , "}"
    , "}"
    ]

run :: IO ()
run = 
    do
        c_nvrtcContainerInit
        nvrtcContainer <- newNvrtcContainer
        b <- compileNvrtcContainer nvrtcContainer (deviceFunc2 <> globalFunc2)
        runNvrtcContainer nvrtcContainer


newNvrtcContainer :: IO NvrtcContainer
newNvrtcContainer = 
    do
        nvrtcContainerFPtr <- c_nvrtcContainerNew >>= newForeignPtr c_nvrtcContainerDelete
        return $ NvrtcContainer nvrtcContainerFPtr

compileNvrtcContainer :: NvrtcContainer -> Text -> IO Bool
compileNvrtcContainer container prog = 
    do
        withForeignPtr (_nvrtcContainerFPtr container) (\containerPtr -> 
            do
                T.withCStringLen prog $ uncurry (c_nvrtcContainerCompile containerPtr)
         )

runNvrtcContainer :: NvrtcContainer -> IO ()
runNvrtcContainer container = 
    do
        withForeignPtr (_nvrtcContainerFPtr container) (\containerPtr -> 
            do
                c_nvrtcContainerRun containerPtr
         )


foreign import ccall safe "nvrtcContainerNew" c_nvrtcContainerNew
    :: IO (Ptr NvrtcContainerHandle)

foreign import ccall safe "nvrtcContainerInit" c_nvrtcContainerInit
    :: IO ()

foreign import ccall safe "nvrtcContainerCompile" c_nvrtcContainerCompile
    :: Ptr NvrtcContainerHandle -> CString -> Int -> IO Bool

foreign import ccall safe "nvrtcContainerRun" c_nvrtcContainerRun
    :: Ptr NvrtcContainerHandle -> IO ()

foreign import ccall safe "&nvrtcContainerDelete" c_nvrtcContainerDelete
    :: FinalizerPtr NvrtcContainerHandle
