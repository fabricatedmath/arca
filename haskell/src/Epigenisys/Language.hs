{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Epigenisys.Language where

import Data.Proxy
import Data.Text (Text)

import Epigenisys.Language.Parser
import Epigenisys.Language.Stack
import Epigenisys.Language.Types

newtype Exec w = Exec (LanguageTree (StackOp w))

instance Show (Exec w) where
    show (Exec (Expression a)) = show a
    show (Exec (Open as)) = "(" ++ unwords (map (show . Exec) as) ++ ")"

class HasEmpty a where
    getEmpty :: a

runWorld :: HasStackLens w (Exec w) => State w ()
runWorld = 
    do
        me <- popL stackLens
        case me of
            Nothing -> return ()
            Just (Exec e) ->
                do
                    case e of
                        Expression a -> stackOpFunc a
                        Open ts -> pushListL stackLens $ map Exec ts
                    runWorld


runLang :: forall w. (HasWorldParser w, HasStackLens w (Exec w), HasEmpty w) => Text -> Either String w
runLang program = 
    do
        opTree <- Exec <$> parseLang Proxy program :: Either String (Exec w)
        let m = pushL stackLens opTree *> runWorld
        return $ execState m getEmpty

