{-# LANGUAGE OverloadedStrings #-}

module Epigenisys.FFI.NvrtcContainer where

import Data.Text (Text)
import qualified Data.Text.Foreign as T

import Foreign.C.String

import Foreign.ForeignPtr
import Foreign.Ptr

import Epigenisys.FFI.CUContextContainer

data NvrtcContainerHandle

data NvrtcContainer =
  NvrtcContainer
  { _nvrtcContainer :: ForeignPtr NvrtcContainerHandle
  }

newNvrtcContainer :: CUContextContainer -> IO NvrtcContainer
newNvrtcContainer cuContextContainerHandle = 
    withForeignPtr (_cuContextContainerHandle cuContextContainerHandle) (\ctxPtr -> 
        NvrtcContainer <$> (c_nvrtcContainerNew ctxPtr >>= newForeignPtr c_nvrtcContainerDelete)
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
