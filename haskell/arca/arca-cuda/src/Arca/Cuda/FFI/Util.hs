module Arca.Cuda.FFI.Util where

import Data.Text (Text)
import qualified Data.Text.Foreign as T (peekCStringLen) 

import Foreign.C.String

import Foreign.ForeignPtr
import Foreign.Ptr

ffiToText :: (Ptr a -> IO CString) -> (Ptr a -> IO Int) -> ForeignPtr a -> IO Text
ffiToText strf strLenf a = withForeignPtr a (ffiToText' strf strLenf)

ffiToText' :: (Ptr a -> IO CString) -> (Ptr a -> IO Int) -> Ptr a -> IO Text
ffiToText' strf strLenf ptr = 
    do
        cstr <- strf ptr
        cstrLen <- strLenf ptr
        T.peekCStringLen (cstr,cstrLen)