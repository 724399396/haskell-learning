type Done = ()

type Strategy a = a -> Done

r0 :: Strategy a
r0 _ = ()

rwhnf :: Strategy a
rwhnf x = x `seq` ()

class NFData a where
  rnf :: Strategy a
  rnf = rwhnf

parList :: Strategy a -> Strategy [a]
parList start [] = ()
parList start (x:xs) = start x `par` (parList start xs)

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap start f xs = map f xs `using` parList start

using :: a -> Strategy a -> a
using x s = s x `seq` x

vectorSum' :: (NFData a, Num a) => [a] -> [a] -> [a]
vectorSum' = parZipWith rnf (+)
