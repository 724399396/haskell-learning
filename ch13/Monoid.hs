{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--class Monoid a where
--  mempty :: a        -- the identity
--  mappend :: a -> a -> -- associative binary operator

--instance Monoid [a] where
--  mempty = []
--  mappend = (++)

import Data.Monoid

newtype AInt = A { unA :: Int}
  deriving (Show, Eq, Num)

-- monoid under addition
instance Monoid AInt where
  mempty = 0
  mappend = (+)

newtype MInt = M { unM :: Int }
  deriving (Show, Eq, Num)

-- monoid under multiplication
instance Monoid MInt where
  mempty = 1
  mappend = (*)
