module Shuffle (shuffle) where

-- Code from: https://wiki.haskell.org/Random_shuffle

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import System.Random

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- fmap (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n =  newListArray (1,n)
