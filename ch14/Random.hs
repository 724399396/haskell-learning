module Random where

import Control.Monad (liftM2)
import Control.Monad.State

import System.Random

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')

type RandomState a = State StdGen a

getRandom :: Random a -> RandomState a
getRandom = 
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val

getTowRandoms :: Random a => RandomState (a, a)
getTowRandoms = liftM2 (,) getRandom getRandom
