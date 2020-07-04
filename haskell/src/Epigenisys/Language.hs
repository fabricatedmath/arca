{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Epigenisys.Language (
    Stack(..), Exec(..), HasEmpty(..), runLang, runProg,
    StackOp(..), textRead, Namespace(..), HasNamespaces(..), NamespaceOps(..)
    , LanguageTree(..), drawLanguageTree, applyNamespace, module Epigenisys.Language.Stack
    , PartialStackOp(..)
    )
where

import Data.Text (Text)
import qualified Data.Text.Lazy as T

import TextShow

import Epigenisys.Language.Internal
import Epigenisys.Language.Parser
import Epigenisys.Language.Stack
import Epigenisys.Language.Tree

newtype Exec w = Exec (LanguageTree (StackOp w))

instance Show (Exec w) where
    show (Exec (Expression a)) = show a
    show (Exec (Open as)) = "(" <> unwords (map (show . Exec) as) <> ")"

instance TextShow (Exec w) where
    showb (Exec (Expression a)) = showb a
    showb (Exec (Open as)) = fromText "(" <> fromLazyText (T.unwords (map (toLazyText . showb . Exec) as)) <> fromText ")"

class HasEmpty a where
    getEmpty :: a

runWorld :: HasStackLens w (Exec w) => State w ()
runWorld = do
    me <- popL stackLens
    case me of
        Nothing -> return ()
        Just (Exec e) -> do
            case e of
                Expression a -> stackOpFunc a
                Open ts -> pushListL stackLens $ map Exec ts
            runWorld

runProg :: forall w. (HasNamespaces w, HasStackLens w (Exec w), HasEmpty w) => Exec w -> w
runProg opTree = do
    let m = pushL stackLens opTree *> runWorld
    execState m getEmpty

runLang :: forall w. (HasNamespaces w, HasStackLens w (Exec w), HasEmpty w) => Text -> Either String w
runLang program = do
    opTree <- Exec <$> parseLang program :: Either String (Exec w)
    pure $ runProg opTree
