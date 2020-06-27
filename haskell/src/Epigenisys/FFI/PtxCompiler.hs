{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Epigenisys.FFI.PtxCompiler (PtxCode(..), ptxCompile) where

import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T (withCStringLen)

import Foreign.C.String

import Foreign.ForeignPtr
import Foreign.Ptr

import TextShow

import Epigenisys.FFI.CudaEnums
import Epigenisys.FFI.Util

data PtxCompilerHandle

newtype PtxCompiler = 
    PtxCompiler 
    { _ptxCompiler :: ForeignPtr PtxCompilerHandle
    }

newtype PtxCode = 
    PtxCode 
    { _ptxCode :: Text
    } deriving (Show, TextShow)

newPtxCompiler :: IO PtxCompiler
newPtxCompiler = fmap PtxCompiler $ c_ptxCompilerNew >>= newForeignPtr c_ptxCompilerDelete

ptxCompile :: (MonadError Text m, MonadIO m) => Text -> m PtxCode
ptxCompile cudaCode = 
    do
        ptxCompiler <- liftIO newPtxCompiler
        eret <- liftIO $ withForeignPtr (_ptxCompiler ptxCompiler) (\ptr ->
            do
                retCode <- toEnum <$> 
                    T.withCStringLen cudaCode (\(str,len) -> 
                        c_ptxCompilerCompile ptr str len False
                    )
                case retCode of
                    NVRTC_SUCCESS -> pure <$> ffiToText' c_ptxCompilerGetPtxStr c_ptxCompilerGetPtxStrLen ptr
                    i -> Left <$> (,) i <$> ffiToText' c_ptxCompilerGetLogStr c_ptxCompilerGetLogStrLen ptr
                )
        case eret of
            Right ptx -> pure $ PtxCode ptx
            Left (result,logText) ->
                do
                    let tabbedLog = T.unlines $ map ("\t" <>) $ T.lines logText
                        codeLines = 
                            T.unlines $ map ("\t" <>) $ 
                            zipWith (\i codeLine -> showt i <> ": " <> codeLine) ([1..] :: [Int]) $ 
                            T.lines $ cudaCode
                    throwError $ 
                        "Failed to compile with code: " <> showt result <> 
                        "\n\n" <> 
                        tabbedLog <> 
                        codeLines

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


