{-# LANGUAGE BangPatterns #-}
import System.Environment
import Text.Printf
import Data.List(foldl')
import qualified Data.Vector as V

data Pair = Pair !Int !Double

main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" (mean (V.enumFromTo 1 d))

mean :: V.Vector Double -> Double
mean xs = s / fromIntegral n
  where
    Pair n s = V.foldl k (Pair 0 0) xs
    k (Pair n s) x = Pair (n+1) (s+x)
