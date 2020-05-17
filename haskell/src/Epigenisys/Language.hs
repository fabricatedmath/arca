{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Epigenisys.Language (
    Stack(..), Exec(..), HasEmpty(..), runLang, HasStack(..),
    HasWorldParser(..), StackType(..), StackOp(..), textRead, StackName(..)
    )
where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Lazy as T

import TextShow

import Epigenisys.Language.Ops 
import Epigenisys.Language.Parser (
    LanguageTree(..), StackOp(..), HasWorldParser(..), parseLang, HasStack(..), StackType(..), textRead,
    StackName(..)
    )
import Epigenisys.Language.Stack

newtype Exec w = Exec (LanguageTree (StackOp w))

instance Show (Exec w) where
    show (Exec (Expression a)) = show a
    show (Exec (Open as)) = "(" ++ unwords (map (show . Exec) as) ++ ")"

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


runLang :: forall w. (HasWorldParser w, HasStackLens w (Exec w), HasEmpty w) => Text -> Either String w
runLang program =  do
    opTree <- Exec <$> parseLang Proxy program :: Either String (Exec w)
    let m = pushL stackLens opTree *> runWorld
    return $ execState m getEmpty

