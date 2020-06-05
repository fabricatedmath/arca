{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Epigenisys.Language.Parser (drawLanguageTree, WorldParserMap, parseText, worldParser, opify)
--import Epigenisys.Worlds.SimpleWorld
import Epigenisys.Worlds.CudaWorld

--import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.IO as T

testProgram1 :: Text
testProgram1 = "((Integer.+ Integer.1 Integer.2 (Integer.3 Float.2 Float.3.5 Float.+) Integer.*))"

testProgram2 :: Text
testProgram2 = "((Integer.3) Integer.+)"

testProgram3 :: Text
testProgram3 = "(INTEGER.1 Integer.+ INTEGER.2 Integer.- )"

testProgram4 :: Text
testProgram4 = "(Integer.1 Integer.2 Integer.+ Integer.convertToFloat2)"

main :: IO ()
main = 
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
        
