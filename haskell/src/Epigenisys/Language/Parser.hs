{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Epigenisys.Language.Parser 
  ( LanguageTree(..), parseLang, textRead, HasNamespaces(..)
  , NamespaceOps(..), drawLanguageTree
  ) where

import Control.Applicative
import Control.Monad (void)

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Data.Char (isSpace)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (Reader)

import GHC.Generics

import TextShow
import TextShow.Generic

import Epigenisys.Language.Types -- (PartialStackOp(..), StackFunc)

type LiteralParser w = Text -> Either String (PartialStackOp w)
data NamespaceOps w = NamespaceOps Namespace (Maybe (LiteralParser w)) [PartialStackOp w]
newtype WorldParserMap w = WorldParserMap2 (Map Namespace (NamespaceParser w))

type NamespaceParser w = Text -> Either String (StackOp w)

applyNamespace :: Namespace -> PartialStackOp w -> StackOp w
applyNamespace namespace (PartialStackOp opName f) = 
  StackOp 
  { stackOpFunc = f
  , stackOpName = OpName $ opName
  , stackOpNamespace = namespace
  }

worldParser :: HasNamespaces w => WorldParserMap w
worldParser = WorldParserMap2 $ Map.fromList $ map buildParserNamespace $ getNamespaces

buildParserNamespace :: NamespaceOps w -> (Namespace, NamespaceParser w)
buildParserNamespace (NamespaceOps namespace mliteralParser partialStackOps) = (namespace, lookupOp)
  where
    opMap = Map.fromList $ map f sops
      where f sop = (unOpName $ stackOpName sop, sop)
            sops = map (applyNamespace namespace) partialStackOps
    lookupOp t = 
      case opMap Map.!? t of
        Just op -> return op
        Nothing -> 
          case mliteralParser of
            Nothing -> Left $ "Failed to parse: " <> show t
            Just literalParser -> 
              do 
                a <- literalParser t
                return $ applyNamespace namespace $ a

opify :: Traversable t => WorldParserMap w -> t StackOpText -> Either String (t (StackOp w))
opify m = traverse (findOp m)
  where
    findOp :: WorldParserMap w -> StackOpText -> Either String (StackOp w)
    findOp (WorldParserMap2 worldMap) (StackOpText stackName' opName') = 
      case worldMap Map.!? stackName' of
        Nothing -> Left $ "Failed to find: " ++ show stackName'
        Just stackParser -> stackParser $ unOpName opName'

class HasNamespaces w where
  getNamespaces :: [NamespaceOps w]

data LanguageTree a = Open (LanguageForest a) | Expression a
  deriving (Generic, Show)
  deriving TextShow via FromGeneric (LanguageTree a)

type LanguageForest a = [LanguageTree a]

instance Functor LanguageTree where
    fmap f (Open as) = Open $ fmap fmap fmap f as
    fmap f (Expression a) = Expression $ f a

instance Foldable LanguageTree where
    foldMap f (Open as) = foldMap (foldMap f) as
    foldMap f (Expression a) = f a

instance Traversable LanguageTree where
    traverse f (Expression a) = Expression <$> f a
    traverse f (Open as) = Open <$> traverse (traverse f) as

drawLanguageTree :: LanguageTree String -> String
drawLanguageTree = unlines . draw
    where
        draw :: LanguageTree String -> [String]
        draw (Expression a) = lines a
        draw (Open as) = lines "Open" ++ drawSubTrees as
            where
                drawSubTrees [] = []
                drawSubTrees [t] =
                    "|" : shift "`- " "   " (draw t)
                drawSubTrees (t:ts) =
                    "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

                shift first other = zipWith (++) (first : repeat other)

parseText :: Text -> Either String (LanguageTree StackOpText)
parseText =  A.parseOnly parser
  where
    parser :: Parser (LanguageTree StackOpText)
    parser = parseParen
        where
            parseParen :: Parser (LanguageTree StackOpText)
            parseParen = do
                void $ A.char '('
                A.skipSpace
                parsed <- parseExpressions
                A.skipSpace
                void $ A.char ')'
                return $ Open parsed

            parseExpressions :: Parser (LanguageForest StackOpText)
            parseExpressions = A.many' parseExpression

            parseExpression :: Parser (LanguageTree StackOpText)
            parseExpression = do
                A.skipSpace
                parseParen <|> parseLiteral

            parseLiteral :: Parser (LanguageTree StackOpText)
            parseLiteral = do
                first <- A.takeWhile1 (\c -> c /= '(' && c /= '.' && not (isSpace c))
                void $ A.char '.'
                second <- A.takeWhile1 (\c -> c /= '(' && c /= ')' && not (isSpace c))
                return $ Expression $ StackOpText (Namespace first) (OpName second)

textRead :: Reader a -> Text -> Either String a
textRead reader t = 
  either (\a -> Left $ "On input " ++ show t ++ ": " ++ a) Right $ do
    (i,t') <- reader t
    if T.null t' 
      then return i
      else Left $ "Failed to consume all input on literal: " ++ show t

parseLang :: HasNamespaces w => Text -> Either String (LanguageTree (StackOp w))
parseLang program = parseText program >>= opify worldParser
    