{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Writer
import MaybeT

promble :: MonadWriter [String] m => m ()
promble = do
  tell ["this is where i fail"]
  fail "oops"

type A = WriterT [String] Maybe

type B = MaybeT (Writer [String])

a :: A()
a = promble

b :: B()
b = promble
