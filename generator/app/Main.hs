{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Arrow 
import Control.Monad (filterM,void)

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Data.Char (isSpace)
import Data.Function (on)

import Data.List (group, groupBy, sort, sortOn)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as T

import GHC.Exts (the)

import Language.Haskell.TH
import System.Directory
import System.FilePath.Posix

listDirectories :: FilePath -> IO [FilePath]
listDirectories dir = 
    do
        entries <- map (dir </>) <$> listDirectory dir
        dirs <- filterM doesDirectoryExist entries
        files <- filterM doesFileExist entries
        fps <- concat <$> mapM listDirectories dirs
        return $ files ++ fps
        
    

buildFile :: FilePath -> FilePath -> IO (Either String ())
buildFile targetDir filepath = 
    do
        let
            splitDirs = splitDirectories filepath
            relevantDirs = tail splitDirs
            directoryPath = targetDir </> joinPath (init relevantDirs)
            moduleName = T.pack $ takeBaseName filepath
            modulePath = T.intercalate "." $ map T.pack $ init relevantDirs
            targetFilename = directoryPath </> T.unpack moduleName <> ".hs"
            moduleHeader = 
                [ "{-# LANGUAGE FlexibleContexts #-}"
                , "{-# LANGUAGE OverloadedStrings #-}"
                , "module " <> modulePath <> "." <> moduleName <> " where"
                , ""
                , "import Epigenisys.Sandbox.Sandbox"
                , "import Epigenisys.Language.Stack"
                , ""
                , "-- | Exported Lists"
                , ""
                ]
            opsSectionHeader = 
                [ "-- | Exported Ops"
                , ""
                ]
        createDirectoryIfMissing True directoryPath
        file <- T.map (\c -> if c == '\8203' then ' ' else  c) <$> T.readFile filepath
        let etext = 
                do
                    funcDefs <- A.parseOnly parseManyFuncDefs $ file
                    let 
                        (t, used) = unzip $ map generateFuncCode funcDefs
                        usedDefs = map fst $ filter snd $ zip funcDefs used

                        -- groupedFuncs: grouped by common set of types (F, FUI, etc..)
                        groupedFuncs :: [([LiteralType], [Text])]
                        groupedFuncs = map (first the . unzip) $ groupBy ((==) `on` fst) $ sortOn fst $ zip (map usedTypes usedDefs) $ map funcDefName usedDefs
                    funcList <- mapM createFuncList groupedFuncs
                    let fullFile = T.unlines moduleHeader <> T.unlines funcList <> T.unlines opsSectionHeader <> T.unlines t
                    return fullFile
        case etext of 
            Left err -> return $ Left err
            Right t -> T.writeFile targetFilename t >> return (pure ())

createFuncList :: ([LiteralType], [Text]) -> Either String Text
createFuncList (litTypes, funcNames) = 
    do
        litNames <- mapM matchType litTypes
        let
            listName = T.toLower $ T.intercalate "_" litNames
            constraint 
                | null litNames = "" 
                | otherwise = "(HasIdentifier w, " <> T.intercalate ", " (map ("HasStackLens w " <>) litNames) <> ") => "
            retType = "[State w ()]"
            typeLine = listName <> " :: " <> constraint <> retType
            defLine = listName <> " = " <> "[" <> T.intercalate ", " funcNames <> "]"
        return $ T.unlines [typeLine, defLine]

compiledDirectory :: String
compiledDirectory = $( stringE =<< (runIO $ getCurrentDirectory) )

main :: IO ()
main = 
    do
        let targetDir = compiledDirectory </> "../haskell/src"
        filesToGen <- listDirectories "descriptions"
        mapM (buildFile targetDir) filesToGen >>= print

usedTypes :: FunctionDef -> [LiteralType]
usedTypes (FunctionDef fretType _fn fargs _comment _originalText) = map the . group $ sort $ fargs ++ [fretType]

data LiteralType = C_Float 
                 | C_Double 
                 | C_Int 
                 | C_UnsignedInt 
                 | C_LongInt
                 | C_LongLongInt 
                 | C_UnsignedLongLongInt 
                 | C_Void
                 | C_ConstChar
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
        A.endOfInput
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
                <|> "long int" *> pure C_LongInt
                <|> "unsigned long long int" *> pure C_UnsignedLongLongInt 
                <|> "float" *> pure C_Float
                <|> "const float" *> pure C_Float
                <|> "double" *> pure C_Double 
                <|> "void" *> pure C_Void
                <|> "__RETURN_TYPE" *> pure C_Int
                <|> "const char" *> pure C_ConstChar

parseArgument :: Parser (LiteralType, Text)
parseArgument = 
    do
        A.skipMany1 A.space
        lit <- parseLiteralTypeOptionPointer
        A.skipMany A.space
        argName <- A.takeTill (\c -> c == ',' || isSpace c || c == ')')
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
                A.skipMany A.space
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
        ia <- convertArgType fargs
        oa <- convertArgType [fretType]
        let
            commentLine = "-- | " <> comment
            typeDecl = fn <> " :: " <> typeConstraints <> retType
                where typeConstraints 
                            | null argTypes = ""
                            | otherwise = "(HasIdentifier w, " <> T.intercalate ", " (map ("HasStackLens w " <>) argTypes) <> ") => "
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
        C_LongInt -> pure "LI"
        C_LongLongInt -> pure "L"
        C_UnsignedLongLongInt -> pure "UL"
        C_Void -> Left "Has Void Type"
        C_Pointer _ -> Left "Has Pointer Type"
        C_ConstChar -> Left "Has Const Char"

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
        argType' [a,b,c,d] = pure $ "FourArg " <> a <> " " <> b <> " " <> c <> " " <> d
        argType' as = Left $ "too many args: " ++ show as 