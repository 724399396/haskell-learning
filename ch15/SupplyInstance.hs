newtype Reader e a = R { runReader :: e -> a }

instance Monad (Reader e) where
  return a = R $ \_ -> a
  m >>= k = R $ \r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id

newtype MySupply e a = MySupply { runMySupply :: Reader e a}

instance MonadSupply e (MySupply e) where
  next = MySupply $ do
           v <- ask
           return (Just v)

  -- more concise:
  -- next = MySupply (Just `liftMa` ask)

xy :: (Num s, MonadSuppy s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)
