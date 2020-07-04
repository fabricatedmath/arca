module Epigenisys.Language.Parser 
  ( parseLang
  ) where

import Control.Applicative
import Control.Monad (void)

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Data.Char (isSpace)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Text (Text)

import Epigenisys.Language.Internal
import Epigenisys.Language.Tree

newtype WorldParserMap w = WorldParserMap (Map Namespace (NamespaceParser w))

type NamespaceParser w = Text -> Either String (StackOp w)

worldParser :: HasNamespaces w => WorldParserMap w
worldParser = WorldParserMap $ Map.fromList $ map buildParserNamespace $ getNamespaces

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
    findOp (WorldParserMap worldMap) (StackOpText stackName' opName') = 
      case worldMap Map.!? stackName' of
        Nothing -> Left $ "Failed to find: " ++ show stackName'
        Just stackParser -> stackParser $ unOpName opName'

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

parseLang :: HasNamespaces w => Text -> Either String (LanguageTree (StackOp w))
parseLang program = parseText program >>= opify worldParser
    