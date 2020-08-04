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
import Data.HashMap (Map)
import qualified Data.HashMap as H

import Data.List (intersperse, intercalate)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import Data.Typeable

import TextShow

newtype UniqueId = UniqueId Int
    deriving (Eq, Hashable, Num, Ord, Show, TextShow)

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
    Accessor :: HasAST o1 => Text -> AST o1 -> AST o
    Call :: UniqueId -> Text -> AST o
    Literal :: o -> AST o
    Variable :: Text -> AST o
    UnaryExpression :: 
        ( HasAST o1
        ) => 
        { astFuncName :: Text
        , astOperand :: AST o1
        } -> AST o
    BinaryExpression ::
        ( HasAST o1, HasAST o2
        ) =>
        { astFuncName :: Text
        , astLeftOp :: AST o1
        , astRightOp :: AST o2
        } -> AST o
    BinaryExpressionInfix ::
        ( HasAST o1, HasAST o2
        ) =>
        { astFuncName :: Text
        , astLeftOp :: AST o1
        , astRightOp :: AST o2
        } -> AST o
    TernaryExpression :: 
        ( HasAST o1, HasAST o2, HasAST o3
        ) => 
        { astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        } -> AST o
    QuaternaryExpression :: 
        ( HasAST o1, HasAST o2, HasAST o3, HasAST o4
        ) => 
        { astFuncName :: Text
        , astOp1 :: AST o1
        , astOp2 :: AST o2
        , astOp3 :: AST o3
        , astOp4 :: AST o4
        } -> AST o

instance HasAST o => Eq (AST o) where
    (==) (Accessor xt x1) (Accessor yt y1) = 
        xt == yt && eqTV x1 y1
    (==) (Call xid xt) (Call yid yt) = 
        xid == yid && xt == yt
    (==) (Literal x1) (Literal y1) = 
        eqTV x1 y1
    (==) (Variable xt) (Variable yt) = 
        xt == yt
    (==) (UnaryExpression xt x1) (UnaryExpression yt y1) = 
        xt == yt && eqTV x1 y1
    (==) (BinaryExpression xt x1 x2) (BinaryExpression yt y1 y2) = 
        xt == yt && eqTV x1 y1 && eqTV x2 y2
    (==) (BinaryExpressionInfix xt x1 x2) (BinaryExpressionInfix yt y1 y2) = 
        xt == yt && eqTV x1 y1 && eqTV x2 y2
    (==) (TernaryExpression xt x1 x2 x3) (TernaryExpression yt y1 y2 y3) = 
        xt == yt && eqTV x1 y1 && eqTV x2 y2 && eqTV x3 y3
    (==) (QuaternaryExpression xt x1 x2 x3 x4) (QuaternaryExpression yt y1 y2 y3 y4) = 
        xt == yt && eqTV x1 y1 && eqTV x2 y2 && eqTV x3 y3 && eqTV x4 y4
    (==) _ _ = False
        
eqTV :: (Eq a, Eq b, Typeable a, Typeable b) => a -> b -> Bool
eqTV a b = maybe False (== a) $ cast b

instance HasAST o => Ord (AST o) where
    compare (Accessor xt x1) (Accessor yt y1) = 
        compare xt yt <> compareTV x1 y1
    compare (Call xid xt) (Call yid yt) = 
        compare xid yid <> compare xt yt
    compare (Literal x1) (Literal y1) = 
        compareTV x1 y1
    compare (Variable xt) (Variable yt) = 
        compare xt yt
    compare (UnaryExpression xt x1) (UnaryExpression yt y1) = 
        compare xt yt <> compareTV x1 y1
    compare (BinaryExpression xt x1 x2) (BinaryExpression yt y1 y2) = 
        compare xt yt <> compareTV x1 y1 <> compareTV x2 y2
    compare (BinaryExpressionInfix xt x1 x2) (BinaryExpressionInfix yt y1 y2) = 
        compare xt yt <> compareTV x1 y1 <> compareTV x2 y2
    compare (TernaryExpression xt x1 x2 x3) (TernaryExpression yt y1 y2 y3) = 
        compare xt yt <> compareTV x1 y1 <> compareTV x2 y2 <> compareTV x3 y3
    compare (QuaternaryExpression xt x1 x2 x3 x4) (QuaternaryExpression yt y1 y2 y3 y4) = 
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
    hashWithSalt s (Accessor t o) = 
        s `hashWithSalt` (0 :: Int) `hashWithSalt` t `hashWithSalt` o
    hashWithSalt s (Call i t) = 
        s `hashWithSalt` (1 :: Int) `hashWithSalt` i `hashWithSalt` t
    hashWithSalt s (Literal o) = 
        s `hashWithSalt` (2 :: Int) `hashWithSalt` o
    hashWithSalt s (Variable t) = 
        s `hashWithSalt` (3 :: Int) `hashWithSalt` t
    hashWithSalt s (UnaryExpression t o1) = 
        s `hashWithSalt` (4 :: Int) `hashWithSalt` t `hashWithSalt` o1
    hashWithSalt s (BinaryExpression t o1 o2) = 
        s `hashWithSalt` (5 :: Int) `hashWithSalt` t `hashWithSalt` o1 `hashWithSalt` o2
    hashWithSalt s (BinaryExpressionInfix t o1 o2) = 
        s `hashWithSalt` (6 :: Int) `hashWithSalt` t `hashWithSalt` o1 `hashWithSalt` o2
    hashWithSalt s (TernaryExpression t o1 o2 o3) = 
        s `hashWithSalt` (7 :: Int) `hashWithSalt` t `hashWithSalt` o1 `hashWithSalt` o2 `hashWithSalt` o3
    hashWithSalt s (QuaternaryExpression t o1 o2 o3 o4) = 
        s `hashWithSalt` (8 :: Int) `hashWithSalt` t `hashWithSalt` o1 `hashWithSalt` o2 `hashWithSalt` o3 `hashWithSalt` o4

instance HasAST o => Show (AST o) where
    show a@(Accessor accessorName o1) = 
        "Accessor(" <> show o1 <> "." <> T.unpack accessorName <> " :: " <> show (typeRep a) <> ")"
    show c@(Call _ident call) = 
        "Call(" <> show call <> " :: " <> show (typeRep c) <> ")"
    show (Literal o) = 
        "Literal(" <> show o <> " :: " <> show (typeOf o) <> ")"
    show v@(Variable s) = 
       "Variable(" <> T.unpack s <> " :: " <> show (typeRep v) <> ")"
    show e@(UnaryExpression name o1) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1]
    show e@(BinaryExpression name o1 o2) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " $ [show o1, show o2]
    show e@(BinaryExpressionInfix name o1 o2) = 
        "( " <> show o1 <> " " <> T.unpack name <> " " <> show o2 <> " )" <> " :: " <> show (typeRep e)
    show e@(TernaryExpression name o1 o2 o3) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1, show o2, show o3]
    show e@(QuaternaryExpression name o1 o2 o3 o4) = 
        T.unpack name <> "( " <> args <> " )" <> " :: " <> show (typeRep e)
            where args = intercalate ", " [show o1, show o2, show o3, show o4]

instance HasAST o => TextShow (AST o) where
    showb a@(Accessor accessorName o1) =
        "Accessor(" <> showb o1 <> "." <> T.fromText accessorName <> " :: " <> showb (typeRep a) <> ")"
    showb c@(Call _ident call) = 
        "Call(" <> T.fromText call <> " :: " <> showb (typeRep c) <> ")"
    showb (Literal o) = 
        "Literal(" <> showb o <> " :: " <> showb (typeOf o) <> ")"
    showb v@(Variable s) = 
       "Variable(" <> T.fromText s <> " :: " <> showb (typeRep v) <> ")"
    showb e@(UnaryExpression name o1) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1]
    showb e@(BinaryExpression name o1 o2) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " $ [showb o1, showb o2]
    showb e@(BinaryExpressionInfix name o1 o2) = 
        "( " <> showb o1 <> " " <> T.fromText name <> " " <> showb o2 <> " )" <> " :: " <> showb (typeRep e)
    showb e@(TernaryExpression name o1 o2 o3) = 
        T.fromText name <> "( " <> args <> " )" <> " :: " <> showb (typeRep e)
            where args = mconcat $ intersperse ", " [showb o1, showb o2, showb o3]
    showb e@(QuaternaryExpression name o1 o2 o3 o4) = 
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
        draw a@(Accessor accessorName o1) = 
            lines $ "Accessor" <> " - " <> show (typeRep o1) <> "." <> show accessorName <> ") :: " <> show (typeOf a)
        draw c@(Call n call) = 
            lines $ "Call" <> " - " <> show n <> " - " <> "(" <> show call <> ") :: " <> show (typeOf c)
        draw (Literal o) = 
            lines $ "Literal" <> " - " <> "(" <> show o <> ") :: " <> show (typeOf o)
        draw v@(Variable s) = 
            lines $ "Variable(" <> show s <> ") :: " <> show (typeOf v)
        draw e@(UnaryExpression name o1) = 
            lines ("UnaryExpression" <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep e]
        draw e@(BinaryExpression name o1 o2) = 
            lines ("BinaryExpression" <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep e]
        draw e@(BinaryExpressionInfix name o1 o2) = 
            lines ("BinaryExpressionInfix" <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep e]
        draw e@(TernaryExpression name o1 o2 o3) = 
            lines ("TernaryExpression" <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2] <> drawSubTrees [o3]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep o3, typeRep e]
        draw e@(QuaternaryExpression name o1 o2 o3 o4) = 
            lines ("QuaternaryExpression" <> " - " <> show name <> " :: " <> args) <> drawSubTrees [o1] <> drawSubTrees [o2] <> drawSubTrees [o3] <> drawSubTrees [o4]
                where args = intercalate " -> " $ map show [typeRep o1, typeRep o2, typeRep o3, typeRep o4, typeRep e]

data Some t = 
    forall a. 
    ( Eq (t a), Hashable (t a), Ord (t a), Typeable (t a)
    ) => Some (t a)

instance Hashable (Some t) where
    hashWithSalt s (Some t) = hashWithSalt s t

instance Eq (Some t) where
    (==) (Some x) (Some y) = eqTV x y

instance Ord (Some t) where
    compare (Some x) (Some y) = compareTV x y

type ASTMap = Map (Some AST) Int
type ASTState = (Int, ASTMap)

type CompileMonad a = RWS () [Text] ASTState a

type NeedsToCompile = Bool
type VarName = Text

-- | Compile AST into c-style statements (compatible with cuda)
compile :: HasAST o => AST o -> (Text, [Text])
compile ast = 
    let 
        (a,s) = evalRWS (compile' ast) () emptyASTState
        codeLines = map (`T.append` ";") $ s
    in (a, codeLines)
    where 
        -- | Looks to see if ast is already checked into map, returns variable name of this 
        -- chunk of code
        checkAndSet :: (C_Type o, MonadState ASTState m) => AST o -> m (NeedsToCompile, VarName)
        checkAndSet ast = 
            do
                let gvn :: Int -> Text
                    gvn n = "a" <> showt n
                (i,m) <- get
                let (mv', m') = H.insertLookupWithKey (\_k _a1 a2 -> a2) (Some ast) i m
                put (i+1, m')
                case mv' of
                    Nothing -> pure (True, gvn i)
                    Just v' -> pure (False, gvn v')

        emptyASTState :: ASTState
        emptyASTState = (0, H.empty)

        compile' :: HasAST o => AST o -> CompileMonad Text
        compile' a@(Accessor accessorName o) =
            do
                (needsToCompile, var) <- checkAndSet a
                when needsToCompile $ 
                    do
                        ovar <- compile' o
                        let lhs = ctypep a <> " " <> var <> " = "
                            rhs = ovar <> "." <> accessorName
                        tell $ pure $ lhs <> rhs
                pure var
        compile' c@(Call _i call) = 
            do
                (needsToCompile, var) <- checkAndSet c
                when needsToCompile $ 
                    tell $ pure $ ctypep c <> " " <> var <> " = " <> call
                pure var
        compile' l@(Literal o) = 
            do
                (needsToCompile, var) <- checkAndSet l
                when needsToCompile $ 
                    tell $ pure $ ctype o <> " " <> var <> " = " <> cval o
                pure var
        compile' (Variable s) = pure s
        compile' e@(UnaryExpression name o) = 
            do
                (needsToCompile, var) <- checkAndSet e
                when needsToCompile $ 
                    do
                        ovar <- compile' o
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = ovar
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(BinaryExpression name o1 o2) = 
            do
                (needsToCompile, var) <- checkAndSet e
                when needsToCompile $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = T.intercalate ", " [ovar1, ovar2]
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(BinaryExpressionInfix name o1 o2) = 
            do
                (needsToCompile, var) <- checkAndSet e
                when needsToCompile $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = ovar1 <> " " <> name <> " " <> ovar2
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(TernaryExpression name o1 o2 o3) = 
            do
                (needsToCompile, var) <- checkAndSet e
                when needsToCompile $ 
                    do
                        ovar1 <- compile' o1
                        ovar2 <- compile' o2
                        ovar3 <- compile' o3
                        let lhs = ctypep e <> " " <> var <> " = "
                            rhs = name <> "(" <> args <> ")"
                                where args = T.intercalate ", " [ovar1, ovar2, ovar3]
                        tell $ pure $ lhs <> rhs
                pure var
        compile' e@(QuaternaryExpression name o1 o2 o3 o4) = 
            do
                (needsToCompile, var) <- checkAndSet e
                when needsToCompile $ 
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

