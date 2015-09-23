returnSingleton :: a -> [a]
returnSingleton x = [x]

instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  xs >> f = concat (map (\_ -> f) xs)
  fail _ = []
