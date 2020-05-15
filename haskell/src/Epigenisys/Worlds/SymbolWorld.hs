
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Epigenisys.Worlds.SymbolWorld where

import Control.Lens

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Read as T (decimal, signed)
import Data.Tree

import Epigenisys.Language
import Epigenisys.Language.Parser
import Epigenisys.Language.Ops
import Epigenisys.Language.Stack
import Epigenisys.Language.Types

newtype AST = 
    AST 
    { unAST :: Tree Op
    } 

instance Show AST where
    show (AST t) = "\n" ++ drawTree (fmap show t)

data Op = Add | Subtract | Multiply | Negate | Abs | Signum | FromInteger Int | Symbol String
    deriving Show

instance Num AST where
    (+) t1 t2 = AST $ Node Add $ map unAST [t1,t2]
    (-) t1 t2 = AST $ Node Subtract $ map unAST [t1,t2]
    (*) t1 t2 = AST $ Node Multiply $ map unAST [t1,t2]
    negate t = AST $ Node Negate $ map unAST [t]
    abs t = AST $ Node Abs $ map unAST [t]
    signum t = AST $ Node Signum $ map unAST [t]
    fromInteger i = AST $ Node (FromInteger $ fromIntegral i) []

data SymbolWorld = 
    SymbolWorld 
    { _execStack :: Stack (Exec SymbolWorld)
    , _astStack :: Stack AST
    } deriving Show

makeLenses ''SymbolWorld

instance HasEmpty SymbolWorld where
    getEmpty = 
        SymbolWorld 
        { _execStack = empty
        , _astStack = empty
        }

instance HasStackLens SymbolWorld (Exec SymbolWorld) where
    stackLens = execStack

instance HasStackLens SymbolWorld AST where
    stackLens = astStack

instance HasStack SymbolWorld (Exec SymbolWorld) where
    stackName _ = StackName "Exec"
    stackOps _ = []
    stackParseLiteral _= Nothing

instance HasStack SymbolWorld AST where
    stackName _ = StackName "Integer"
    stackOps _ = 
        [ addOp astProxy
        , multiplyOp astProxy
        ] where astProxy = Proxy :: Proxy AST
    stackParseLiteral _ = 
        let
            ei = textRead $ T.signed T.decimal :: Text -> Either String Int
        in Just $ fmap (\i -> AST $ Node (FromInteger i) []). ei

instance HasWorldParser SymbolWorld where
    worldTypes = 
        [ StackType (Proxy :: Proxy (SymbolWorld, Exec SymbolWorld))
        , StackType (Proxy :: Proxy (SymbolWorld, AST))
        ]

