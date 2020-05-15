{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Epigenisys.Language.Parser where

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

import Data.Proxy

import Epigenisys.Language.Ops (literalOp)
import Epigenisys.Language.Types

data LanguageTree a = Open (LanguageForest a) | Expression a
  deriving Show

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

data StackOpText = StackOpText StackName StackOpName
    deriving Show

newtype StackOpName = StackOpName { unStackOpName :: Text }
    deriving (Eq, Ord, Show)

newtype StackName = StackName { unStackName :: Text }
  deriving (Eq, Ord, Show)

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
                return $ Expression $ StackOpText (StackName first) (StackOpName second)

class (HasStackLens w a, Show a) => HasStack w a where
  stackName :: Proxy (w,a) -> StackName
  stackOps :: Proxy (w,a) -> [PartialStackOp w]
  stackParseLiteral :: Proxy (w,a) -> Maybe (Text -> Either String a)

applyOp :: forall w a. HasStack w a => Proxy (w,a) -> PartialStackOp w -> StackOp w
applyOp proxy (PartialStackOp opName stackFunc) =
  StackOp 
  { stackOpFunc = stackFunc
  , stackOpName = StackOpName opName
  , stackOpStackName = stackName proxy
  }

data StackOp w = 
  StackOp 
  { stackOpFunc :: StackFunc w
  , stackOpName :: StackOpName
  , stackOpStackName :: StackName
  }

instance Show (StackOp w) where
  show sop = 
    T.unpack $ 
    unStackName (stackOpStackName sop) 
    `T.append` "." `T.append` 
    unStackOpName (stackOpName sop)

newtype WorldParserMap w = WorldParserMap (Map StackName (StackParser w))

type StackParser w = StackOpName -> Either String (StackOp w)

opify :: Traversable t => WorldParserMap w -> t StackOpText -> Either String (t (StackOp w))
opify m = traverse (findOp m)
  where
    findOp :: WorldParserMap w -> StackOpText -> Either String (StackOp w)
    findOp (WorldParserMap worldMap) (StackOpText stackName' opName') = 
      case worldMap Map.!? stackName' of
        Nothing -> Left $ "Failed to find: " ++ show stackName'
        Just stackParser -> stackParser opName'

worldParser :: HasWorldParser w => WorldParserMap w 
worldParser = WorldParserMap $ Map.fromList $ map makeStackParser $ worldTypes
  where
    makeStackParser :: StackType w -> (StackName, StackParser w)
    makeStackParser (StackType p) = (stackName p, lookupOp)
      where
        opMap = Map.fromList $ map f ops
          where f sop = (stackOpName sop, sop)
                ops = map (applyOp p) $ stackOps p

        lookupOp t = 
          case opMap Map.!? t of
            Just op' -> return op'
            Nothing -> 
              case stackParseLiteral p of
                Nothing -> Left $ "Failed to parse: " ++ show t
                Just literalParser ->
                  do
                    a <- literalParser $ unStackOpName t
                    return $ applyOp p $ literalOp a Proxy

textRead :: Reader a -> Text -> Either String a
textRead reader t = 
  either (\a -> Left $ "On input " ++ show t ++ ": " ++ a) Right $ do
    (i,t') <- reader t
    if T.null t' 
      then return i
      else Left $ "Failed to consume all input on literal: " ++ show t

data StackType w = forall a. HasStack w a => StackType (Proxy (w,a))

class HasWorldParser w where
  worldTypes :: [StackType w]

-- | Need to refactor this or prune unneccesary functions
printWorldStackOps :: forall w. HasWorldParser w => Proxy w -> String
printWorldStackOps _ = show $ map getStackOps worldTypes
  where
    getStackOps :: StackType w -> [StackOp w]
    getStackOps (StackType p) = map (applyOp p) $ stackOps p

parseLang :: HasWorldParser w => Proxy w -> Text -> Either String (LanguageTree (StackOp w))
parseLang _ program = 
  do
    textTree <- parseText program
    opTree <- opify worldParser textTree
    return opTree
    