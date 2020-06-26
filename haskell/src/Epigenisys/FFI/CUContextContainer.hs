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
    fmap CUContextContainer $ c_cuContextContainerNew >>= newForeignPtr c_cuContextContainerDelete

setCurrentContext :: CUContextContainer -> IO ()
setCurrentContext cuContextContainer = 
    withForeignPtr (_cuContextContainerHandle cuContextContainer) c_cuContextContainerSetCurrentContext

popContext :: CUContextContainer -> IO ()
popContext cuContextContainer = 
    withForeignPtr (_cuContextContainerHandle cuContextContainer) c_cuContextContainerPopContext

foreign import ccall unsafe "cuContextContainerNew" c_cuContextContainerNew
    :: IO (Ptr CUContextContainerHandle)

foreign import ccall unsafe "cuContextContainerSetCurrentContext" c_cuContextContainerSetCurrentContext
    :: Ptr CUContextContainerHandle -> IO ()

foreign import ccall unsafe "cuContextContainerPopContext" c_cuContextContainerPopContext
    :: Ptr CUContextContainerHandle -> IO ()

foreign import ccall unsafe "&cuContextContainerNew" c_cuContextContainerDelete
    :: FinalizerPtr CUContextContainerHandle
