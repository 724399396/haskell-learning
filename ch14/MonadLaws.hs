fmap id = id
fmap (f . g) = fmap f . fmap g

return x >>= f          === f x

do y <- return x
   f y          ===  f x

m >>= return       ===  m

do y <- m
   return y        === m

m >>= (\x -> f x >>= g)   === (m >>= f) >>= g

m >>= s
  where s x = f x >>= g

t >>= g
  where t = m >>= f


