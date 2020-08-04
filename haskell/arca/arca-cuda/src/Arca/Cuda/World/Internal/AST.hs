{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arca.Cuda.World.Internal.AST 
    ( AST(..), C_Type(..)
    , compile, drawASTString
    , ratchetId, initUniqueId, HasIdentifier(..), UniqueId,
    HasAST, compareTV
    ) where

import Control.Lens
import Control.Monad.RWS.Strict

import Data.Hashable
import Data.List (intersperse, intercalate)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import Data.Typeable

import TextShow

newtype UniqueId = UniqueId Int
    deriving (Num, Show, TextShow)

initUniqueId :: UniqueId
initUniqueId = UniqueId 0

class HasIdentifier w where
    idLens :: Lens' w UniqueId

ratchetId :: MonadState w m => HasIdentifier w => m UniqueId
ratchetId = idLens %%= (\i -> (i,i+1))

ctype :: C_Type a => a -> Text
ctype = ctypep . Just

class (Eq a, Hashable a, Ord a, Show a, TextShow a, Typeable a) => C_Type a where
    -- | Name of c type in c language, e.g. "unsigned long long int"
    ctypep :: proxy a -> Text
    -- | Text representation of value, e.g. "395" or "2"
    cval :: a -> Text

type HasAST o = (C_Type o, Eq o, Hashable o, Ord o, Show o, TextShow o, Typeable o)

-- | Consider making Text a type variable to support embedding of haskell land values (automatic differentiation)

-- | Can get rid of UniqueId by checking equality of AST?

-- | Boundable ast for while loop? Takes an AST as an argument, maybe?

data AST o where
    Accessor :: HasAST o1 => UniqueId -> Text -> AST o1 -> AST o
    Call :: UniqueId -> Text -> AST o
    Literal :: UniqueId -> o -> AST o
    Variable :: Text -> AST o
    UnaryExpression :: 
        ( HasAST o1
        ) => 
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astOperand :: AST o1
        } -> AST o
    BinaryExpression ::
        ( HasAST o1, HasAST o2
        ) =>
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astLeftOp :: AST o1
        , astRightOp :: AST o2
        } -> AST o
    BinaryExpressionInfix ::
        ( HasAST o1, HasAST o2
        ) =>
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astLeftOp :: AST o1
        , astRightOp :: AST o2
        } -> AST o
    TernaryExpression :: 
        ( HasAST o1, HasAST o2, HasAST o3
        ) => 
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        } -> AST o
    QuaternaryExpression :: 
        ( HasAST o1, HasAST o2, HasAST o3, HasAST o4
        ) => 
        { astOpId :: UniqueId
        , astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        , astOp4 :: AST o4
        } -> AST o

instance HasAST o => Eq (AST o) where
    (==) (Accessor xid xt x1) (Accessor yid yt y1) = 
        xt == yt && eqTV x1 y1
    (==) (Call xid xt) (Call yid yt) = 
        xt == yt
    (==) (Literal xid x1) (Literal yid y1) = 
        eqTV x1 y1
    (==) (Variable xt) (Variable yt) = 
        xt == yt
    (==) (UnaryExpression xid xt x1) (UnaryExpression yid yt y1) = 
        xt == yt && eqTV x1 y1
    (==) (BinaryExpression xid xt x1 x2) (BinaryExpression yid yt y1 y2) = 
        xt == yt && eqTV x1 y1 && eqTV x2 y2
    (==) (BinaryExpressionInfix xid xt x1 x2) (BinaryExpressionInfix yid yt y1 y2) = 
        xt == yt && eqTV x1 y1 && eqTV x2 y2
    (==) (TernaryExpression xid xt x1 x2 x3) (TernaryExpression yid yt y1 y2 y3) = 
        xt == yt && eqTV x1 y1 && eqTV x2 y2 && eqTV x3 y3
    (==) (QuaternaryExpression xid xt x1 x2 x3 x4) (QuaternaryExpression yid yt y1 y2 y3 y4) = 
        xt == yt && eqTV x1 y1 && eqTV x2 y2 && eqTV x3 y3 && eqTV x4 y4
    (==) _ _ = False
        
eqTV :: (Eq a, Eq b, Typeable a, Typeable b) => a -> b -> Bool
eqTV a b = maybe False (== a) $ cast b

instance HasAST o => Ord (AST o) where
    compare (Accessor xid xt x1) (Accessor yid yt y1) = 
        compare xt yt <> compareTV x1 y1
    compare (Call xid xt) (Call yid yt) = 
        compare xt yt
    compare (Literal xid x1) (Literal yid y1) = 
        compareTV x1 y1
    compare (Variable xt) (Variable yt) = 
        compare xt yt
    compare (UnaryExpression xid xt x1) (UnaryExpression yid yt y1) = 
        compare xt yt <> compareTV x1 y1
    compare (BinaryExpression xid xt x1 x2) (BinaryExpression yid yt y1 y2) = 
        compare xt yt <> compareTV x1 y1 <> compareTV x2 y2
    compare (BinaryExpressionInfix xid xt x1 x2) (BinaryExpressionInfix yid yt y1 y2) = 
        compare xt yt <> compareTV x1 y1 <> compareTV x2 y2
    compare (TernaryExpression xid xt x1 x2 x3) (TernaryExpression yid yt y1 y2 y3) = 
        compare xt yt <> compareTV x1 y1 <> compareTV x2 y2 <> compareTV x3 y3
    compare (QuaternaryExpression xid xt x1 x2 x3 x4) (QuaternaryExpression yid yt y1 y2 y3 y4) = 
        compare xt yt <> compareTV x1 y1 <> compareTV x2 y2 <> compareTV x3 y3 <> compareTV x4 y4
    compare Accessor{} _ = LT
    compare _ Accessor{} = GT
    compare Call{} _ = LT
    compare _ Call{} = GT
    compare Literal{} _ = LT
    compare _ Literal{} = GT
    compare Variable{} _ = LT
    compare _ Variable{} = GT
    compare UnaryExpression{} _ = LT
    compare _ UnaryExpression{} = GT
    compare BinaryExpression{} _ = LT
    compare _ BinaryExpression{} = GT
    compare BinaryExpressionInfix{} _ = LT
    compare _ BinaryExpressionInfix{} = GT
    compare TernaryExpression{} _ = LT
    compare _ TernaryExpression{} = GT
    --compare QuaternaryExpression{} _ = LT
    --compare _ QuaternaryExpression{} = GT

compareTV :: (Ord a, Ord b, Typeable a, Typeable b) => a -> b -> Ordering
compareTV a b = maybe (compare (typeOf a) (typeOf b)) (compare a) $ cast b

instance Hashable o => Hashable (AST o) where
    hashWithSalt s (Accessor id t o) = 
        s `hashWithSalt` (0 :: Int) `hashWithSalt` t `hashWithSalt` o
    hashWithSalt s (Call id t) = 
        s `hashWithSalt` (1 :: Int) `hashWithSalt` t
    hashWithSalt s (Literal id o) = 
        s `hashWithSalt` (2 :: Int) `hashWithSalt` o
    hashWithSalt s (Variable t) = 
        s `hashWithSalt` (3 :: Int) `hashWithSalt` t
    hashWithSalt s (UnaryExpression id t o1) = 
        s `hashWithSalt` (4 :: Int) `hashWithSalt` t `hashWithSalt` o1
    hashWithSalt s (BinaryExpression id t o1 o2) = 
        s `hashWithSalt` (5 :: Int) `hashWithSalt` t `hashWithSalt` o1 `hashWithSalt` o2
    hashWithSalt s (BinaryExpressionInfix id t o1 o2) = 
        s `hashWithSalt` (6 :: Int) `hashWithSalt` t `hashWithSalt` o1 `hashWithSalt` o2
    hashWithSalt s (TernaryExpression id t o1 o2 o3) = 
        s `hashWithSalt` (7 :: Int) `hashWithSalt` t `hashWithSalt` o1 `hashWithSalt` o2 `hashWithSalt` o3
    hashWithSalt s (QuaternaryExpression id t o1 o2 o3 o4) = 
        s `hashWithSalt` (8 :: Int) `hashWithSalt` t `hashWithSalt` o1 `hashWithSalt` o2 `hashWithSalt` o3 `hashWithSalt` o4

instance HasAST o => Show (AST o) where
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
    show e@(TernaryExpression _ident name o1 o2 o3) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1, show o2, show o3]
    show e@(QuaternaryExpression _ident name o1 o2 o3 o4) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1, show o2, show o3, show o4]

instance HasAST o => TextShow (AST o) where
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
    showb e@(TernaryExpression _ident name o1 o2 o3) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1, showb o2, showb o3]
    showb e@(QuaternaryExpression _ident name o1 o2 o3 o4) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1, showb o2, showb o3, showb o4]

-- | Represents AST as multiline tabbed strings expressions
drawASTString :: HasAST o => AST o -> String
drawASTString = unlines . draw
    where
        drawSubTrees [] = []
        drawSubTrees [t] = 
            "|" : shift "`- " "   " (draw t)
        drawSubTrees (t:ts) =
            "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)

        draw :: HasAST o => AST o -> [String]
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
        draw e@(TernaryExpression n name o1 o2 o3) = 
            lines ("TernaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2] <> drawSubTrees [o3]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep o3, typeRep e]
        draw e@(QuaternaryExpression n name o1 o2 o3 o4) = 
            lines ("QuaternaryExpression" <> " - " <> show n <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2] <> drawSubTrees [o3] <> drawSubTrees [o4]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep o3, typeRep o4, typeRep e]

type CompileMonad a = RWS () [Text] IntSet a

-- | Compile AST into c-style statements (compatible with cuda)
compile :: HasAST o => AST o -> (Text, [Text])
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

        compile' :: HasAST o => AST o -> CompileMonad Text
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
        compile' e@(TernaryExpression i name o1 o2 o3) = 
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
        compile' e@(QuaternaryExpression i name o1 o2 o3 o4) = 
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

