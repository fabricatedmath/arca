{-# LANGUAGE DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Arca.Cuda.World where

import Control.Lens

import Data.Array

import Data.Maybe (listToMaybe)

import Data.Proxy

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T (decimal, signed, rational)

import Data.Typeable

import GHC.Generics

import TextShow
import TextShow.Generic

import System.Random

import Arca.Language
import Arca.Language.Mutate

import Arca.Cuda.World.Internal
import Arca.Cuda.World.Ops
import Arca.Cuda.World.VectorOps

import qualified Arca.Cuda.World.GeneratedOps.IntegerIntrinsics 
    as IntegerIntrinsics

import qualified Arca.Cuda.World.GeneratedOps.SinglePrecisionMathematicalFunctions 
    as SinglePrecisionMathematicalFunctions
    
import qualified Arca.Cuda.World.GeneratedOps.SinglePrecisionIntrinsics 
    as SinglePrecisionIntrinsics

data World = 
    World
    { _execStack :: Stack (Exec World)
    , _intStack :: Stack I
    , _floatStack :: Stack F
    , _float2Stack :: Stack F2
    , _float4Stack :: Stack F4
    , _identifier :: UniqueId
    } deriving (Generic, Show)
      deriving TextShow via FromGeneric World

makeLenses ''World

instance HasEmpty World where
    getEmpty = 
        World 
        { _execStack = empty
        , _intStack = empty
        , _floatStack = empty
        , _float2Stack = empty
        , _float4Stack = empty
        , _identifier = initUniqueId
        }

instance HasIdentifier World where
    idLens = identifier

instance HasStackLens World (Exec World) where
    stackLens = execStack

instance HasStackLens World I where
    stackLens = intStack

instance HasStackLens World F where
    stackLens = floatStack

instance HasStackLens World F2 where
    stackLens = float2Stack

instance HasStackLens World F4 where
    stackLens = float4Stack

randGenCallOp :: StackOp World
randGenCallOp = psoToSo (Namespace "Float") $ callOp fp (OpName "rgen") floatRGenCudaCall
    where 
        fp = Proxy :: Proxy C_Float
        floatRGenCudaCall = "curand(&localState)"

usesCurand :: Foldable t => t (StackOp World) -> Bool
usesCurand = any (\x -> any (== x) randStackOps)

randStackOps :: [StackOp World]
randStackOps = [randGenCallOp]

cudaWorldOps :: Array Int (StackOp World)
cudaWorldOps = listArray (0,length stackops - 1) stackops
    where
        (NamespaceOps namespace _ pops) = cudaNamespaceOps
        stackops = map (psoToSo namespace) pops

execWorldOps :: Array Int (StackOp World)
execWorldOps = listArray (0,length stackops - 1) stackops
    where
        (NamespaceOps namespace _ pops) = execNamespaceOps
        stackops = map (psoToSo namespace) pops

randomElementSampler :: forall g m. (MonadState g m, RandomGen g) => m (StackOp World)
randomElementSampler =
    do
        let
            genFloat :: m (StackOp World)
            genFloat = do
                r <- state $ random
                return $ psoToSo (Namespace "Float") $ literalOp . C_Float $ r

            genInt :: m (StackOp World)
            genInt = do
                r <- state $ random
                return $ psoToSo (Namespace "Int") $ literalOp . C_Int $ r

            genVariable :: m (StackOp World)
            genVariable = pure randGenCallOp

            genLiteral :: m (StackOp World)
            genLiteral = do
                b <- state $ random
                case b of
                    True -> genFloat
                    False -> genInt

            genOp :: m (StackOp World)
            genOp = do
                let thresh = 0.3 :: Double
                r <- state $ random
                case () of 
                    _ | r < thresh -> genCudaOp
                      | otherwise -> genExecOp
                            
            genCudaOp :: m (StackOp World)
            genCudaOp = do
                let arr = cudaWorldOps
                r <- state $ randomR $ bounds arr
                return $ arr ! r

            genExecOp :: m (StackOp World)
            genExecOp = do
                let arr = execWorldOps
                r <- state $ randomR $ bounds arr
                return $ arr ! r

        let thresh = 0.6 :: Double
        r <- state $ random
        case () of 
            _ | r < thresh -> genOp
              -- | otherwise -> genFloat
              | otherwise -> genVariable

cudaNamespaceOps :: NamespaceOps World
cudaNamespaceOps = NamespaceOps (Namespace "Cuda") Nothing ops
    where
        ops = mconcat $ 
                [ --IntegerIntrinsics.i
                --, SinglePrecisionMathematicalFunctions.f_i
                SinglePrecisionMathematicalFunctions.f
                , SinglePrecisionIntrinsics.f
                ]

intNamespaceOps :: NamespaceOps World
intNamespaceOps = NamespaceOps (Namespace "Int") litParser intOps
    where 
        litParser = Just $ fmap (literalOp . C_Int) . textRead (T.signed T.decimal)
        intOps = 
            [ addOp ip
            , subtractOp ip
            , multiplyOp ip
            , divideOp ip
            ] where ip = Proxy :: Proxy C_Int

floatNamespaceOps :: NamespaceOps World
floatNamespaceOps = NamespaceOps (Namespace "Float") litParser floatOps
    where 
        litParser = Just $ fmap (literalOp . C_Float) . textRead (T.signed T.rational)
        floatOps = 
            [ addOp fp
            , subtractOp fp
            , multiplyOp fp
            , divideOp fp
            , soToPso randGenCallOp
            , dupOpAST fp
            ] where fp = Proxy :: Proxy C_Float

vectorNamespaceOps :: [NamespaceOps World]
vectorNamespaceOps = 
    [ NamespaceOps (Namespace "Float2") Nothing [packF2, unpackF2]
    , NamespaceOps (Namespace "Float4") Nothing [packF4, unpackF4]
    ]

execNamespaceOps :: NamespaceOps World
execNamespaceOps = NamespaceOps (Namespace "Exec") Nothing [dupOp execProxy]
    where execProxy = Proxy :: Proxy (Exec World)

instance HasNamespaces World where
    getNamespaces = 
        [ cudaNamespaceOps
        , intNamespaceOps
        , floatNamespaceOps
        , execNamespaceOps
        ] <> vectorNamespaceOps

doStuff2 :: Int -> IO () 
doStuff2 limit = 
    do
        let prog = "(Exec.dup (Float.1 Float.rgen Float.*) Float.+ Float.dup Float.+)"
            ew = parseLang prog :: Either String (LanguageTree (StackOp World))
        case ew of 
            Left err -> print err
            Right t -> 
                do
                    print $ usesCurand t
                    putStrLn $ drawLanguageTree $ fmap show t
                    T.putStr . printWorld $ runProg limit $ Exec t

doStuff :: Int -> IO () 
doStuff limit = 
    do
        let float4Ex = "(Float.1 Float.2 Float.3 Float.4 Float4.make_float4 Float4.unpack_float4 Float.+ Float.+ Float.+)"
            float2Ex = "(Float.1 Float.2 Float2.make_float2 Float2.unpack_float2 Float.*)"
            prog = "(" <> float4Ex <> " " <> float2Ex <> " Float./" <> ")"
            ew = parseLang prog :: Either String (LanguageTree (StackOp World))
        case ew of 
            Left err -> print err
            Right t -> 
                do
                    print $ usesCurand t
                    putStrLn $ drawLanguageTree $ fmap show t
                    T.putStr . printWorld $ runProg limit $ Exec t

runStuff :: Int -> IO ()
runStuff limit = 
    do
        let s = randomElementSampler
        g <- newStdGen 
        let t = evalState (generateLanguageTree s 10 30) g :: LanguageTree (StackOp World)
        putStrLn $ drawLanguageTree $ fmap show t
        T.putStrLn $ showt $ Exec t
        T.putStr . printWorld $ runProg limit $ Exec t

generateRandomWorld :: Int -> IO World
generateRandomWorld limit = 
    do
        let s = randomElementSampler
        g <- newStdGen 
        let t = evalState (generateLanguageTree s 10 30) g :: LanguageTree (StackOp World)
        --putStrLn $ drawLanguageTree $ fmap show t
        --T.putStrLn $ showt $ Exec t
        return $ runProg limit $ Exec t

{-
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
        T.putStrLn $ snd $ compile $ u2
        putStrLn $ drawASTString b2
        T.putStrLn $ snd $ compile $ b2
        putStrLn $ drawASTString b4
        T.putStrLn $ snd $ compile $ b4
        -}

testProgram :: Text 
testProgram = "(Int.2 Int.3 Float.3.0 Float.6.9 Cuda.__hadd Cuda.acosf Cuda.__fadd_rn Cuda.isnan Float.69999)"

testProgram2 :: Text
testProgram2 = "(Exec.dup (Int.1 Int.2))"

testProgram3 :: Text
testProgram3 = "(Float.1 Float.2 Float.+ Float.8 Float./ Int.1 Int.3 Int.*)"

printWorld :: World -> Text
printWorld w = T.intercalate "\n" $ filter (not . T.null) [printStacks (_intStack w), printStacks (_floatStack w), printStacks (_float4Stack w)]

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
        f (i,ast) = 
                tabLines $ 
                stackTypeName <> " Stack Element " <> showt i <> "\n\n" <> 
                tabLines (T.pack (drawASTString ast)) <> "\n" <> 
                (T.unlines . map ("\t" <>) $ codeLines <> ["return " <> a <> ";"])
                    where (a,codeLines) = compile ast
        
tabLines :: Text -> Text
tabLines = T.unlines . map ("\t" <>) . T.lines

generateACudaProgram :: World -> Maybe [Text]
generateACudaProgram w = 
    do
        expression <- listToMaybe $ unStack $ _floatStack w
        let (a, codeLines) = compile expression
        return $ codeLines <> ["return " <> a <> ";"]

doOtherStuff :: Int -> IO () 
doOtherStuff limit = 
    do
        let ew = runLang limit testProgram3 :: Either String World
        either putStrLn (T.putStr . printWorld) ew
