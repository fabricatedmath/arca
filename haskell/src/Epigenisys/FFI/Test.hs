module Epigenisys.FFI.Test where

import Foreign.C.String

testFunc :: String -> IO ()
testFunc s = 
    do
        withCAString s c_test

foreign import ccall safe "test" c_test
    :: CString -> IO ()