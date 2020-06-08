{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Epigenisys.Language.Mutate where

import Control.Monad.State.Strict

import Data.List (sort)
import qualified Data.IntSet as S

import System.Random 
import Data.Proxy 
import Data.Tree 

import Epigenisys.Language.Parser

class RandomSampler a b | b -> a where
    randomElement :: MonadState StdGen m => m b

generateLanguageTree :: RandomSampler a b => Int -> Int -> State StdGen (LanguageTree b)
generateLanguageTree bf num = generateTree bf num >>= replaceTree
    where 
        replaceTree (Node _ []) = Expression <$> randomElement
        replaceTree (Node _ xs) = Open <$> mapM replaceTree xs


generateTree :: Int -> Int -> State StdGen (Tree Int)
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

        sumAndBins :: MonadState StdGen m => Int -> Int -> m [Int]
        sumAndBins s 1 = pure $ [s]
        sumAndBins s b = sampleUnique (b-1) (1,s+b-1)
            where
                sampleUnique :: (MonadState StdGen m) => Int -> (Int,Int) -> m [Int]
                sampleUnique n range@(l,h) = f . S.toList <$> sampleUnique' (S.fromAscList [l-1,h+1])
                    where 
                        f xs = zipWith (\x y -> y - x - 1) xs $ tail xs
                        sampleUnique' s = do
                            r <- state $ randomR range
                            let s' = S.insert r s
                            if S.size s' == (n+2) then return s' else sampleUnique' s' 
