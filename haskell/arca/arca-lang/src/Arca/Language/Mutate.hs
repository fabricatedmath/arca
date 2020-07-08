{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arca.Language.Mutate where

import Control.Monad.State.Strict

import qualified Data.IntSet as S

import System.Random 
import Data.Tree 

import Arca.Language.Tree

-- | Generate a random language sampler, using sampler arg
generateLanguageTree 
    :: (MonadState g m, RandomGen g)
    => m a -- ^ Sampler
    -> Int -- ^ Branching factor
    -> Int -- ^ Num leaf nodes
    -> m (LanguageTree a)
generateLanguageTree sampler bf num = generateTree bf num >>= replaceTree
    where 
        replaceTree (Node _ []) = Expression <$> sampler
        replaceTree (Node _ xs) = Open <$> mapM replaceTree xs

-- | Generate a random tree, contains sums to be overwritten at each node
generateTree 
    :: forall g m. (MonadState g m, RandomGen g)
    => Int -- ^ Branching Factor
    -> Int -- ^ Num leaf nodes
    -> m (Tree Int)
generateTree bf = generateTree'
    where 
        generateTree' n
            | n == 1 = pure $ Node n []
            | otherwise = 
                do
                    r <- state $ randomR (1,bf)
                    ls <- filter (/= 0) <$> sumAndBins n r
                    trees <- mapM generateTree' ls
                    pure $ Node n trees

        sumAndBins :: Int -> Int -> m [Int]
        sumAndBins total 1 = pure $ [total]
        sumAndBins total b = sampleUnique (b-1) (1,total+b-1)
            where
                sampleUnique :: Int -> (Int,Int) -> m [Int]
                sampleUnique n range@(l,h) = f . S.toList <$> sampleUnique' (S.fromAscList [l-1,h+1])
                    where 
                        f xs = zipWith (\x y -> y - x - 1) xs $ tail xs
                        sampleUnique' s = do
                            r <- state $ randomR range
                            let s' = S.insert r s
                            if S.size s' == (n+2) then return s' else sampleUnique' s' 
