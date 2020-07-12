{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arca.Language (
    Stack(..), Exec(..), HasEmpty(..), runLang, runProg,
    StackOp(..), textRead, Namespace(..), HasNamespaces(..), NamespaceOps(..)
    , LanguageTree(..), drawLanguageTree, applyNamespace, module Arca.Language.Stack
    , PartialStackOp(..), parseLang, OpName(..)
    )
where

import Data.Text (Text)
import qualified Data.Text.Lazy as T

import TextShow

import Arca.Language.Internal
import Arca.Language.Parser
import Arca.Language.Stack
import Arca.Language.Tree

newtype Exec w = Exec (LanguageTree (StackOp w))

instance Show (Exec w) where
    show (Exec (Expression a)) = show a
    show (Exec (Open as)) = "(" <> unwords (map (show . Exec) as) <> ")"

instance TextShow (Exec w) where
    showb (Exec (Expression a)) = showb a
    showb (Exec (Open as)) = fromText "(" <> fromLazyText (T.unwords (map (toLazyText . showb . Exec) as)) <> fromText ")"

class HasEmpty a where
    getEmpty :: a

runWorld :: HasStackLens w (Exec w) => Int -> State w ()
runWorld limit 
    | limit <= 0 = pure ()
    | otherwise = do
        me <- popL stackLens
        case me of
            Nothing -> pure ()
            Just (Exec e) -> do
                case e of
                    Expression a -> stackOpFunc a
                    Open ts -> pushListL stackLens $ map Exec ts
                runWorld (limit - 1)

runProg :: forall w. (HasNamespaces w, HasStackLens w (Exec w), HasEmpty w) => Int -> Exec w -> w
runProg limit opTree = do
    let m = pushL stackLens opTree *> runWorld limit
    execState m getEmpty

runLang :: forall w. (HasNamespaces w, HasStackLens w (Exec w), HasEmpty w) => Int -> Text -> Either String w
runLang limit program = do
    opTree <- Exec <$> parseLang program :: Either String (Exec w)
    pure $ runProg limit opTree
