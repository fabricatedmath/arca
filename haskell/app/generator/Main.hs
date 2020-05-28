{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Arrow 
import Control.Monad (void)

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Data.Char (isSpace)
import Data.Function (on)

import Data.List (group, groupBy, sort, sortOn)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T

import GHC.Exts (the)



createFuncList :: ([LiteralType], [Text]) -> Either String Text
createFuncList (litTypes, funcNames) = 
    do
        litNames <- mapM matchType litTypes
        let
            listName = T.toLower $ T.concat litNames
            constraint 
                | null litNames = "" 
                | otherwise = "(" <> T.intercalate ", " (map ("HasStackLens w " <>) litNames) <> ") => "
            retType = "[State w ()]"
            typeLine = listName <> " :: " <> constraint <> retType
            defLine = listName <> " = " <> "[" <> T.intercalate ", " funcNames <> "]"
        return $ T.unlines [typeLine, defLine]

main :: IO ()
main = 
    do
        file <- T.map (\c -> if c == '\8203' then ' ' else  c) <$> T.readFile "cuda.txt"
        case A.parseOnly parseManyFuncDefs $ file of
            Left err -> print err
            Right funcDefs -> 
                let 
                    (t, used) = unzip $ map generateFuncCode funcDefs
                    usedDefs = map fst $ filter snd $ zip funcDefs used

                    -- groupedFuncs: grouped by common set of types (F, FUI, etc..)
                    groupedFuncs :: [([LiteralType], [Text])]
                    groupedFuncs = map (first the . unzip) $ groupBy ((==) `on` fst) $ sortOn fst $ zip (map usedTypes usedDefs) $ map funcDefName usedDefs
                in do
                    T.putStr $ T.unlines t
                    either print T.putStr $ T.unlines <$> mapM createFuncList groupedFuncs
                    --print $ map (first the . unzip) $ groupBy ((==) `on` fst) $ sortOn fst $ zip (map usedTypes usedDefs) $ map funcDefName usedDefs

usedTypes :: FunctionDef -> [LiteralType]
usedTypes (FunctionDef fretType _fn fargs _comment _originalText) = map the . group $ sort $ fargs ++ [fretType]

data LiteralType = C_Float 
                 | C_Double 
                 | C_Int 
                 | C_UnsignedInt 
                 | C_LongLongInt 
                 | C_UnsignedLongLongInt 
                 | C_Void
                 | C_Pointer LiteralType
    deriving (Eq, Ord, Show)

data FunctionDef = 
    FunctionDef 
    { funcDefRetType :: LiteralType
    , funcDefName :: Text
    , funcDefArgs :: [LiteralType]
    , funcDefComment :: Text
    , funcOriginalText :: Text
    } deriving Show

parseManyFuncDefs :: Parser [FunctionDef]
parseManyFuncDefs = 
    do
        funcDefs <- A.many1 parseFuncDef
        void $ A.skipMany (void A.space <|> A.endOfLine)
        return funcDefs

parseLiteralTypeOptionPointer :: Parser LiteralType
parseLiteralTypeOptionPointer = 
    do
        t <- parseLiteralType
        A.option t $ A.char '*' *> pure (C_Pointer t)

parseLiteralType :: Parser LiteralType
parseLiteralType =  "int" *> pure C_Int
                <|> "unsigned int" *> pure C_UnsignedInt 
                <|> "long long int" *> pure C_LongLongInt 
                <|> "unsigned long long int" *> pure C_UnsignedLongLongInt 
                <|> "float" *> pure C_Float 
                <|> "double" *> pure C_Double 
                <|> "void" *> pure C_Void
                <|> "__RETURN_TYPE" *> pure C_Int

parseArgument :: Parser (LiteralType, Text)
parseArgument = 
    do
        A.skipMany1 A.space
        lit <- parseLiteralTypeOptionPointer
        A.skipMany1 A.space
        argName <- A.takeTill (\c -> c == ',' || isSpace c)
        return (lit, argName)

parseFuncDef :: Parser FunctionDef
parseFuncDef = (\(t,p) -> p t) <$> A.match parseFuncDef'
    where
        parseFuncDef' :: Parser (Text -> FunctionDef)
        parseFuncDef' = 
            do
                void $ A.string "__device__"
                A.skipMany1 A.space
                retType <- parseLiteralType
                A.skipMany1 A.space
                A.option () $ void $ A.string "__CRTDECL" *> A.skipMany1 A.space
                funcName <- A.takeTill isSpace
                A.skipMany1 A.space
                void $ A.char '('
                args <- map fst <$> A.sepBy parseArgument (A.char ',')
                A.skipMany1 A.space
                void $ A.char ')'
                void $ A.takeTill A.isEndOfLine
                A.endOfLine
                comment <- A.takeTill A.isEndOfLine
                A.endOfLine
                return $ FunctionDef retType funcName args $ T.strip comment

generateFuncCode :: FunctionDef -> (Text, Bool)
generateFuncCode fdef = 
    case generateFuncCode' fdef of
        Left err -> 
            let 
                e = "-- | Failed to parse " <> "(" <> T.pack err <> "):" <> "\n"
                o = T.unlines $ map ("-- | \t" <>) $ T.lines $ funcOriginalText fdef
            in (T.concat [e,"-- |\n", o], False)
        Right code -> (code, True)

generateFuncCode' :: FunctionDef -> Either String Text
generateFuncCode' fdef@(FunctionDef fretType fn fargs comment _originalText) = 
    do
        argTypes <- mapM matchType $ usedTypes fdef
        ia <- convertArgType [fretType]
        oa <- convertArgType fargs
        let
            commentLine = "-- | " <> comment
            typeDecl = fn <> " :: " <> typeConstraints <> retType
                where typeConstraints 
                            | null argTypes = ""
                            | otherwise = "(" <> T.intercalate ", " (map ("HasStackLens w " <>) argTypes) <> ") => "
                      retType = "State w ()"
            funcDecl = fn <> " = " <> "opify (" <> op <> ")"
                where op = "Op False " <> ("\"" <> fn <> "\"") <> " :: Op " <> ia <> " " <> oa
        return $ T.unlines [commentLine, typeDecl, funcDecl]

matchType :: LiteralType -> Either String Text
matchType l = 
    case l of
        C_Float -> pure "F"
        C_Double -> pure "D"
        C_Int -> pure "I"
        C_UnsignedInt -> pure "UI"
        C_LongLongInt -> pure "L"
        C_UnsignedLongLongInt -> pure "UL"
        C_Void -> Left "Has Void Type"
        C_Pointer _ -> Left "Has Pointer Type"

convertArgType :: [LiteralType] -> Either String Text
convertArgType ts = 
    do
        args <- mapM matchType ts >>= argType'
        pure $ "(" <> args <> ")"
    where
        argType' :: [Text] -> Either String Text
        argType' [a] = pure $ "OneArg " <> a
        argType' [a,b] = pure $ "TwoArg " <> a <> " " <> b
        argType' [a,b,c] = pure $ "ThreeArg " <> a <> " " <> b <> " " <> c
        argType' as = Left $ "too many args: " ++ show as 