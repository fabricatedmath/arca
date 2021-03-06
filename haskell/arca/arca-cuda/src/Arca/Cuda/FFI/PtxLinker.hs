{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.FFI.PtxLinker (FuncName(..), ptxLink, PtxRunner, ptxRun) where

import Control.Monad.Except

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T (withCStringLen)

import Foreign.C.String

import Foreign.ForeignPtr
import Foreign.Ptr

import TextShow

import Arca.Cuda.FFI.CudaEnums
import Arca.Cuda.FFI.PtxCompiler
import Arca.Cuda.FFI.Util

data PtxLinkerHandle

newtype PtxLinker = 
    PtxLinker 
    { _ptxLinker :: ForeignPtr PtxLinkerHandle
    }

newtype PtxRunner = 
    PtxRunner 
    { _ptxRunner :: ForeignPtr PtxLinkerHandle
    }

newPtxLinker :: IO PtxLinker
newPtxLinker = fmap PtxLinker $ c_ptxLinkerNew >>= newForeignPtr c_ptxLinkerDelete

newtype FuncName = 
    FuncName 
    { _funcName :: Text 
    } deriving (IsString, Show)

-- TODO handle case where logs aren't written!
ptxLink :: (MonadError Text m, MonadIO m) => FuncName -> PtxCode -> m (Text, PtxRunner)
ptxLink (FuncName funcName) (PtxCode ptxCode) = 
    do
        ptxLinkerHandle <- liftIO $ _ptxLinker <$> newPtxLinker
        eret <- liftIO $
            withForeignPtr ptxLinkerHandle (\ptr ->
                T.withCStringLen ptxCode (\(ptxStr, ptxStrLen) ->
                    T.withCStringLen funcName (\(funcNameStr, funcNameStrLen) ->
                        do
                            retCode <- cuResultIntToCode <$> c_ptxLinkerLink ptr ptxStr ptxStrLen funcNameStr funcNameStrLen
                            case retCode of
                                CUDA_SUCCESS -> pure <$> ffiToText' c_ptxLinkerGetInfoLogStr c_ptxLinkerGetInfoLogStrLen ptr
                                i -> Left <$> (,) i <$> ffiToText' c_ptxLinkerGetErrorLogStr c_ptxLinkerGetErrorLogStrLen ptr
                    )
                )
            )
        case eret of
            Right logText -> pure $ (logText, PtxRunner ptxLinkerHandle)
            Left (result,logText) -> 
                do
                    let tabbedLog = T.unlines $ map ("\t" <>) $ T.lines logText
                    {- Prints generated ptx as part of error message
                        codeLines = 
                            T.unlines $ map ("\t" <>) $ 
                            zipWith (\i ptxLine -> showt i <> ": " <> ptxLine) ([1..] :: [Int]) $ 
                            T.lines $ ptxCode
                    -}
                    throwError $ 
                        "Failed to link with code: " <> showt result <> 
                        "\n\n" <> 
                        tabbedLog
                        -- <> codeLines

ptxRun :: (MonadError Text m, MonadIO m) => PtxRunner -> Int -> Int -> m ()
ptxRun ptxRunner numBlocks numThreads = 
    do
        retCode <- liftIO $ cuResultIntToCode <$>
            withForeignPtr (_ptxRunner $ ptxRunner) (\ptr -> 
                c_ptxLinkerRun ptr numBlocks numThreads
            )
        case retCode of
            CUDA_SUCCESS -> pure ()
            result -> throwError $ "Failed to run with code: " <> showt result

foreign import ccall unsafe "ptxLinkerNew" c_ptxLinkerNew
    :: IO (Ptr PtxLinkerHandle)

foreign import ccall unsafe "ptxLinkerLink" c_ptxLinkerLink
    :: Ptr PtxLinkerHandle -> CString -> Int -> CString -> Int -> IO Int

foreign import ccall unsafe "ptxLinkerRun" c_ptxLinkerRun
    :: Ptr PtxLinkerHandle -> Int -> Int -> IO Int

foreign import ccall unsafe "ptxLinkerGetInfoLogStr" c_ptxLinkerGetInfoLogStr
    :: Ptr PtxLinkerHandle -> IO CString

foreign import ccall unsafe "ptxLinkerGetInfoLogStrLen" c_ptxLinkerGetInfoLogStrLen
    :: Ptr PtxLinkerHandle -> IO Int

foreign import ccall unsafe "ptxLinkerGetErrorLogStr" c_ptxLinkerGetErrorLogStr
    :: Ptr PtxLinkerHandle -> IO CString

foreign import ccall unsafe "ptxLinkerGetErrorLogStrLen" c_ptxLinkerGetErrorLogStrLen
    :: Ptr PtxLinkerHandle -> IO Int

foreign import ccall unsafe "&ptxLinkerDelete" c_ptxLinkerDelete
    :: FinalizerPtr PtxLinkerHandle