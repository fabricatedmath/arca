{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.Internal.AST 
    ( AST(..), C_Type(..)
    , compile, drawASTString
    , ratchetId, initUniqueId, HasIdentifier(..), UniqueId
    ) where

import Control.Lens
import Control.Monad.RWS.Strict

import Data.List (intersperse, intercalate)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import Data.Typeable

import TextShow

import Arca.Language

newtype UniqueId = UniqueId Int
    deriving (Num, Show, TextShow)

initUniqueId :: UniqueId
initUniqueId = UniqueId 0

class HasIdentifier w where
    idLens :: Lens' w UniqueId

ratchetId :: HasIdentifier w => State w UniqueId
ratchetId = idLens %%= (\i -> (i,i+1))

ctype :: C_Type a => a -> Text
ctype = ctypep . Just

class (Show a, TextShow a, Typeable a) => C_Type a where
    -- | Name of c type in c language, e.g. "unsigned long long int"
    ctypep :: proxy a -> Text
    -- | Text representation of value, e.g. "395" or "2"
    cval :: a -> Text

data AST o where
    Accessor :: 
        (C_Type o1, Show o1, TextShow o1, Typeable o1
        ) => UniqueId -> Text -> AST o1 -> AST o
    Call :: UniqueId -> Text -> AST o
    Literal :: UniqueId -> o -> AST o
    Variable :: Text -> AST o
    UnaryExpression :: 
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        ) => 
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astOperand :: AST o1
        } -> AST o
    BinaryExpression ::
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        , C_Type o2, Show o2, TextShow o2, Typeable o2
        ) =>
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astLeftOp :: AST o1
        , astRightOp :: AST o2
        } -> AST o
    BinaryExpressionInfix ::
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        , C_Type o2, Show o2, TextShow o2, Typeable o2
        ) =>
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astLeftOp :: AST o1
        , astRightOp :: AST o2
        } -> AST o
    TrinaryExpression :: 
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        , C_Type o2, Show o2, TextShow o2, Typeable o2
        , C_Type o3, Show o3, TextShow o3, Typeable o3
        ) => 
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        } -> AST o
    QuadrinaryExpression :: 
        ( C_Type o1, Show o1, TextShow o1, Typeable o1
        , C_Type o2, Show o2, TextShow o2, Typeable o2
        , C_Type o3, Show o3, TextShow o3, Typeable o3
        , C_Type o4, Show o4, TextShow o4, Typeable o4
        ) => 
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        , astOp4 :: AST o4
        } -> AST o

instance (Show o, Typeable o) => Show (AST o) where
    show a@(Accessor _ident accessorName o1) = 
        "Accessor(" <> show o1 <> "." <> T.unpack accessorName <> " :: " <> show (typeRep a) <> ")"
    show c@(Call _ident call) = 
        "Call(" <> show call <> " :: " <> show (typeRep c) <> ")"
    show (Literal _ident o) = 
        "Literal(" <> show o <> " :: " <> show (typeOf o) <> ")"
    show v@(Variable s) = 
       "Variable(" <> T.unpack s <> " :: " <> show (typeRep v) <> ")"
    show e@(UnaryExpression _ident name o1) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1]
    show e@(BinaryExpression _ident name o1 o2) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " $ [show o1, show o2]
    show e@(BinaryExpressionInfix _ident name o1 o2) = 
        "( " <> show o1 <> " " <> T.unpack name <> " " <> show o2 <> " )" <> " :: " <> show (typeRep e)
    show e@(TrinaryExpression _ident name o1 o2 o3) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1, show o2, show o3]
    show e@(QuadrinaryExpression _ident name o1 o2 o3 o4) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1, show o2, show o3, show o4]

instance (TextShow o, Typeable o) => TextShow (AST o) where
    showb a@(Accessor _ident accessorName o1) =
        "Accessor(" <> showb o1 <> "." <> T.fromText accessorName <> " :: " <> showb (typeRep a) <> ")"
    showb c@(Call _ident call) = 
        "Call(" <> T.fromText call <> " :: " <> showb (typeRep c) <> ")"
    showb (Literal _ident o) = 
        "Literal(" <> showb o <> " :: " <> showb (typeOf o) <> ")"
    showb v@(Variable s) = 
       "Variable(" <> T.fromText s <> " :: " <> showb (typeRep v) <> ")"
    showb e@(UnaryExpression _ident name o1) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1]
    showb e@(BinaryExpression _ident name o1 o2) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " $ [showb o1, showb o2]
    showb e@(BinaryExpressionInfix _ident name o1 o2) = 
        "( " <> showb o1 <> " " <> T.fromText name <> " " <> showb o2 <> " )" <> " :: " <> showb (typeRep e)
    showb e@(TrinaryExpression _ident name o1 o2 o3) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1, showb o2, showb o3]
    showb e@(QuadrinaryExpression _ident name o1 o2 o3 o4) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1, showb o2, showb o3, showb o4]

-- | Represents AST as multiline tabbed strings expressions
drawASTString :: (Show o, Typeable o) => AST o -> String
drawASTString = unlines . draw
    where
        drawSubTrees [] = []
        drawSubTrees [t] = 
            "|" : shift "`- " "   " (draw t)
        drawSubTrees (t:ts) =
            "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)

        draw :: (Show o, Typeable o) => AST o -> [String]
        draw a@(Accessor n accessorName o1) = 
            lines $ "Accessor" <> " - " <> show n <> " - " <> show (typeRep o1) <> "." <> show accessorName <> ") :: " <> show (typeOf a)
        draw c@(Call n call) = 
            lines $ "Call" <> " - " <> show n <> " - " <> "(" <> show call <> ") :: " <> show (typeOf c)
        draw (Literal n o) = 
            lines $ "Literal" <> " - " <> show n <> " - " <> "(" <> show o <> ") :: " <> show (typeOf o)
        draw v@(Variable s) = 
            lines $ "Variable(" <> show s <> ") :: " <> show (typeOf v)
        draw e@(UnaryExpression n name o1) = 
            lines ("UnaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep e]
        draw e@(BinaryExpression n name o1 o2) = 
            lines ("BinaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep e]
        draw e@(BinaryExpressionInfix n name o1 o2) = 
            lines ("BinaryExpressionInfix" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep e]
        draw e@(TrinaryExpression n name o1 o2 o3) = 
            lines ("TrinaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2] <> drawSubTrees [o3]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep o3, typeRep e]
        draw e@(QuadrinaryExpression n name o1 o2 o3 o4) = 
            lines ("QuadrinaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2] <> drawSubTrees [o3] <> drawSubTrees [o4]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep o3, typeRep o4, typeRep e]

type CompileMonad a = RWS () [Text] IntSet a

-- | Compile AST into c-style statements (compatible with cuda)
compile :: (C_Type o, TextShow o, Typeable o) => AST o -> (Text, [Text])
compile ast = 
    let 
        (a,s) = evalRWS (compile' ast) () S.empty
        codeLines = map (`T.append` ";") $ s
    in (a, codeLines)
    where 
        gvn :: UniqueId -> Text
        gvn (UniqueId n) = "a" <> showt n

        checkAndAdd :: MonadState IntSet m => UniqueId -> m Bool
        checkAndAdd (UniqueId i) =
            do
                s <- get
                put $ S.insert i s
                pure $ S.member i s

        compile' :: (C_Type o, TextShow o, Typeable o) => AST o -> CompileMonad Text
        compile' a@(Accessor i accessorName o) =
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar <- compile' o
                        let lhs = ctypep a <> " " <> var <> " = "
                            rhs = ovar <> "." <> accessorName
                        tell $ pure $ lhs <> rhs
                pure var
        compile' c@(Call i call) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ tell $ pure $ ctypep c <> " " <> var <> " = " <> call
                pure var
        compile' (Literal i o) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ tell $ pure $ ctype o <> " " <> var <> " = " <> cval o
                pure var
        compile' (Variable s) = pure s
        compile' e@(UnaryExpression i name o) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar <- compile' o
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = ovar
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(BinaryExpression i name o1 o2) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = T.intercalate ", " [ovar1, ovar2]
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(BinaryExpressionInfix i name o1 o2) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = ovar1 <> " " <> name <> " " <> ovar2
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(TrinaryExpression i name o1 o2 o3) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        ovar3 <- compile' o3
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = T.intercalate ", " [ovar1, ovar2, ovar3]
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(QuadrinaryExpression i name o1 o2 o3 o4) = 
            do
                b <- checkAndAdd i
                let var = gvn i
                unless b $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        ovar3 <- compile' o3
                        ovar4 <- compile' o4
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = T.intercalate ", " [ovar1, ovar2, ovar3, ovar4]
                        tell $ pure $ lhs <> rhs
                pure var

