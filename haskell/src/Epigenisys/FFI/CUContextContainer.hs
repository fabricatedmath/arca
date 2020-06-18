module Epigenisys.FFI.CUContextContainer where

import Foreign.ForeignPtr
import Foreign.Ptr

data CUContextContainerHandle

data CUContextContainer =
  CUContextContainer
  { _cuContextContainerHandle :: ForeignPtr CUContextContainerHandle
  }

newCUContextContainer :: IO CUContextContainer
newCUContextContainer = 
    do
        cuContextContainerHandle <- c_cuContextContainerNew >>= newForeignPtr c_cuContextContainerDelete
        return $ CUContextContainer cuContextContainerHandle

foreign import ccall unsafe "cuContextContainerNew" c_cuContextContainerNew
    :: IO (Ptr CUContextContainerHandle)

foreign import ccall unsafe "&cuContextContainerNew" c_cuContextContainerDelete
    :: FinalizerPtr CUContextContainerHandle
