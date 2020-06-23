{-# LANGUAGE OverloadedStrings #-}

module Epigenisys.FFI.PtxCompiler where

import Data.Text (Text)
import qualified Data.Text.Foreign as T (withCStringLen)

import Foreign.C.String

import Foreign.ForeignPtr
import Foreign.Ptr

import Epigenisys.FFI.Util

data PtxCompilerHandle

data PtxCompiler = 
    PtxCompiler 
    { _ptxCompiler :: ForeignPtr PtxCompilerHandle
    }

newPtxCompiler :: IO PtxCompiler
newPtxCompiler = 
    do
        ptxCompilerHandle <- c_ptxCompilerNew >>= newForeignPtr c_ptxCompilerDelete
        return $ PtxCompiler ptxCompilerHandle

ptxCompile :: Text -> IO (Either (NvrtcResult,Text) Text)
ptxCompile cudaCode = 
    do
        ptxCompiler <- newPtxCompiler
        withForeignPtr (_ptxCompiler ptxCompiler) (\ptr ->
            do
                retCode <- toEnum <$> 
                    T.withCStringLen cudaCode (\(str,len) -> 
                        c_ptxCompilerCompile ptr str len False
                    )
                case retCode of
                    NVRTC_SUCCESS -> Right <$> ffiToText' c_ptxCompilerGetPtxStr c_ptxCompilerGetPtxStrLen ptr
                    i -> Left <$> (,) i <$> ffiToText' c_ptxCompilerGetLogStr c_ptxCompilerGetLogStrLen ptr
            )


foreign import ccall unsafe "ptxCompilerNew" c_ptxCompilerNew
    :: IO (Ptr PtxCompilerHandle)

foreign import ccall unsafe "ptxCompilerCompile" c_ptxCompilerCompile
    :: Ptr PtxCompilerHandle -> CString -> Int -> Bool -> IO Int

foreign import ccall unsafe "ptxCompilerGetPtxStr" c_ptxCompilerGetPtxStr
    :: Ptr PtxCompilerHandle -> IO CString

foreign import ccall unsafe "ptxCompilerGetPtxStrLen" c_ptxCompilerGetPtxStrLen
    :: Ptr PtxCompilerHandle -> IO Int

foreign import ccall unsafe "ptxCompilerGetLogStr" c_ptxCompilerGetLogStr
    :: Ptr PtxCompilerHandle -> IO CString

foreign import ccall unsafe "ptxCompilerGetLogStrLen" c_ptxCompilerGetLogStrLen
    :: Ptr PtxCompilerHandle -> IO Int

foreign import ccall unsafe "&ptxCompilerDelete" c_ptxCompilerDelete
    :: FinalizerPtr PtxCompilerHandle

data NvrtcResult = 
    NVRTC_SUCCESS
    | NVRTC_ERROR_OUT_OF_MEMORY
    | NVRTC_ERROR_PROGRAM_CREATION_FAILURE
    | NVRTC_ERROR_INVALID_INPUT
    | NVRTC_ERROR_INVALID_PROGRAM
    | NVRTC_ERROR_INVALID_OPTION
    | NVRTC_ERROR_COMPILATION
    | NVRTC_ERROR_BUILTIN_OPERATION_FAILURE
    | NVRTC_ERROR_NO_NAME_EXPRESSIONS_AFTER_COMPILATION
    | NVRTC_ERROR_NO_LOWERED_NAMES_BEFORE_COMPILATION
    | NVRTC_ERROR_NAME_EXPRESSION_NOT_VALID
    | NVRTC_ERROR_INTERNAL_ERROR
        deriving (Show, Enum)
