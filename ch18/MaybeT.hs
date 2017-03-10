{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses
           , UndecidableInstances #-}
module MaybeT where

import Control.Monad.Trans
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

newtype MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
  }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
  unwrapped <- runMaybeT x
  case unwrapped of
    Nothing -> return Nothing
    Just y -> runMaybeT (f y)

x `altBindMt` f =
  MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT

instance Monad m => Applicative (MaybeT m) where
  pure = returnMT
  (<*>) = undefined

instance Monad m => Functor (MaybeT m) where
  fmap f a = (pure f) <*> a

instance MonadTrans MaybeT where
  lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)

instance (MonadWriter s m) => MonadWriter s (MaybeT m) where
  tell x = lift (tell x)
  listen = undefined
  pass = undefined
