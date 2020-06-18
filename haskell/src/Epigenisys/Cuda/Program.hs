{-# LANGUAGE OverloadedStrings #-}

module Epigenisys.Cuda.Program where

import Data.Text (Text)
import qualified Data.Text as T

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