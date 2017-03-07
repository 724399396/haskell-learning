{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
  (
    Supply,
    next,
    runSupply
  ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a)
  deriving (Monad,Applicative,Functor)

runSupply :: Supply s a -> [s] -> (a, [s])

next :: Supply s (Maybe s)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

{- instance Monad (Supply s) where
  s >>= m = S (unwrapS s >>= unwrapS . m)
  return = S . return -}

next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply (S m) xs = runState m xs

showTwo :: (Show s) => Supply s String
showTwo = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b" ++ show b)
