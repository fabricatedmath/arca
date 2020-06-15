module Epigenisys.FFI.Test where

import Data.Text (Text)
import qualified Data.Text.Foreign as T

import Foreign.C.String

testFunc :: Text -> IO ()
testFunc s = 
    do
        T.withCStringLen s $ uncurry c_test
                --withCAString s c_test

foreign import ccall safe "test" c_test
    :: CString -> Int -> IO ()