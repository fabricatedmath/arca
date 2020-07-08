{-# LANGUAGE DeriveGeneric, DerivingVia #-}

module Arca.Language.Tree where

import GHC.Generics

import TextShow
import TextShow.Generic

data LanguageTree a = Open (LanguageForest a) | Expression a
  deriving (Generic, Show)
  deriving TextShow via FromGeneric (LanguageTree a)

type LanguageForest a = [LanguageTree a]

instance Functor LanguageTree where
    fmap f (Open as) = Open $ fmap fmap fmap f as
    fmap f (Expression a) = Expression $ f a

instance Foldable LanguageTree where
    foldMap f (Open as) = foldMap (foldMap f) as
    foldMap f (Expression a) = f a

instance Traversable LanguageTree where
    traverse f (Expression a) = Expression <$> f a
    traverse f (Open as) = Open <$> traverse (traverse f) as

drawLanguageTree :: LanguageTree String -> String
drawLanguageTree = unlines . draw
    where
        draw :: LanguageTree String -> [String]
        draw (Expression a) = lines a
        draw (Open as) = lines "Open" ++ drawSubTrees as
            where
                drawSubTrees [] = []
                drawSubTrees [t] =
                    "|" : shift "`- " "   " (draw t)
                drawSubTrees (t:ts) =
                    "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

                shift first other = zipWith (++) (first : repeat other)