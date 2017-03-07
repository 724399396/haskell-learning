import Control.Monad

{- guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero -}

x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
