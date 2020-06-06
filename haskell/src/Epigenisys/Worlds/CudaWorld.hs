{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Epigenisys.Worlds.CudaWorld where

import Control.Lens

import Data.Proxy

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T (decimal, signed, rational)

import GHC.Generics

import TextShow
import TextShow.Generic

import Epigenisys.Language
--import Epigenisys.Language.Ops
import Epigenisys.Language.Parser
import Epigenisys.Language.Stack
import Epigenisys.Language.Types
import Epigenisys.Worlds.CudaWorld.Internal.CudaWorld
import qualified Epigenisys.Worlds.CudaWorld.GeneratedOps.IntegerIntrinsics as IntegerIntrinsics

data World = 
    World
    { _execStack :: Stack (Exec World)
    , _intStack :: Stack I
    , _identifier :: Int
    } deriving (Generic, Show)
      deriving TextShow via FromGeneric World

makeLenses ''World

instance HasEmpty World where
    getEmpty = 
        World 
        { _execStack = empty
        , _intStack = empty
        , _identifier = 0
        }

instance HasIdentifier World where
    idLens = identifier

instance HasStackLens World (Exec World) where
  stackLens = execStack

instance HasStackLens World I where
    stackLens = intStack

intNamespaceOps :: NamespaceOps World
intNamespaceOps = NamespaceOps (Namespace "Int") literalParser IntegerIntrinsics.i
    where
        intProxy = Proxy :: Proxy I
        literalParser = Just $ fmap (literalOp . C_Int) . textRead (T.signed T.decimal)

instance HasNamespaces World where
    getNamespaces = 
        [ intNamespaceOps
        ]

doStuff :: IO ()
doStuff = 
    do
        let
            l1 = Literal 8 2 :: AST C_Int
            l2 = Literal 9 3 :: AST C_Int
            b = BinaryExpression 1 False "add" l1 l2 :: AST C_Int
            u = UnaryExpression 2 "abs" b :: AST C_Int
            t = TrinaryExpression 3 "addAll" b l1 u :: AST C_Int
            v = Variable "laneId" :: AST C_Int
            b2 = BinaryExpression 4 False "mul" t v :: AST C_Int
        print b2
        putStrLn $ drawASTString b2
        let
            u2 = UnaryExpression 5 "abs" l1 :: AST C_Int
            b3 = BinaryExpression 6 True "+" u2 u2 :: AST C_UnsignedLongLongInt
            b4 = BinaryExpression 7 True "*" b3 u2 :: AST C_Int
        T.putStrLn $ compile $ u2
        putStrLn $ drawASTString b2
        T.putStrLn $ compile $ b2
        putStrLn $ drawASTString b4
        T.putStrLn $ compile $ b4

testProgram :: Text 
testProgram = "(Int.2)"
        
doOtherStuff :: IO () 
doOtherStuff = 
    do
        printT $ (runLang testProgram :: Either String World)