{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Epigenisys.Worlds.CudaWorld where

import Control.Lens

import Data.Proxy

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T (decimal, signed, rational)

import Data.Typeable

import GHC.Generics

import TextShow
import TextShow.Generic

import Epigenisys.Language
import Epigenisys.Language.Parser
import Epigenisys.Language.Stack
import Epigenisys.Worlds.CudaWorld.Internal.CudaWorld
import qualified Epigenisys.Worlds.CudaWorld.GeneratedOps.IntegerIntrinsics as IntegerIntrinsics
import qualified Epigenisys.Worlds.CudaWorld.GeneratedOps.SinglePrecisionMathematicalFunctions as SinglePrecisionMathematicalFunctions
import qualified Epigenisys.Worlds.CudaWorld.GeneratedOps.SinglePrecisionIntrinsics as SinglePrecisionIntrinsics

data World = 
    World
    { _execStack :: Stack (Exec World)
    , _intStack :: Stack I
    , _floatStack :: Stack F
    , _identifier :: Int
    } deriving (Generic, Show)
      deriving TextShow via FromGeneric World

makeLenses ''World

instance HasEmpty World where
    getEmpty = 
        World 
        { _execStack = empty
        , _intStack = empty
        , _floatStack = empty
        , _identifier = 0
        }

instance HasIdentifier World where
    idLens = identifier

instance HasStackLens World (Exec World) where
  stackLens = execStack

instance HasStackLens World I where
    stackLens = intStack

instance HasStackLens World F where
    stackLens = floatStack

cudaNamespaceOps :: NamespaceOps World
cudaNamespaceOps = NamespaceOps (Namespace "Cuda") Nothing ops
    where ops = mconcat $ 
                [ IntegerIntrinsics.i
                , SinglePrecisionMathematicalFunctions.f_i
                , SinglePrecisionMathematicalFunctions.f
                , SinglePrecisionIntrinsics.f
                ]

intNamespaceOps :: NamespaceOps World
intNamespaceOps = NamespaceOps (Namespace "Int") litParser []
    where litParser = Just $ fmap (literalOp . C_Int) . textRead (T.signed T.decimal)

floatNamespaceOps :: NamespaceOps World
floatNamespaceOps = NamespaceOps (Namespace "Float") litParser []
    where litParser = Just $ fmap (literalOp . C_Float) . textRead (T.signed T.rational)

instance HasNamespaces World where
    getNamespaces = 
        [ cudaNamespaceOps
        , intNamespaceOps
        , floatNamespaceOps
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
testProgram = "(Int.2 Int.3 Float.3.0 Float.6.9 Cuda.__hadd Cuda.acosf Cuda.__fadd_rn Cuda.isnan Float.69999)"

printWorld :: World -> Text
printWorld w = T.intercalate "\n" $ filter (not . T.null) [printStacks (_intStack w), printStacks (_floatStack w)]

printStacks :: forall o. (C_Type o, Show o, TextShow o, Typeable o) => Stack (AST o) -> Text
printStacks s = 
    "----------------\n" <> 
    stackTypeName <> " Stack\n" <>
    "----------------\n\n" <> 
    string
    where 
        stackTypeName = showt (typeRep (Proxy :: Proxy o))
        string = T.intercalate "\n" . map f . zip [0..] $ unStack $ s
        f :: (Int, AST o) -> Text
        f (i,ast) = tabLines $ 
            stackTypeName <> " Stack Element " <> showt i <> "\n\n" <> 
            tabLines (T.pack (drawASTString ast)) <> "\n" <> 
            tabLines (compile ast)
        
tabLines :: Text -> Text
tabLines = T.unlines . map ("\t" <>) . T.lines

doOtherStuff :: IO () 
doOtherStuff = 
    do
        let ew = runLang testProgram :: Either String World
        either putStrLn (T.putStr . printWorld) ew
