{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Control.Monad.Crdt where

import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.Crdt
import Data.Monoid ((<>))


class (MonadReader s m, CanUpdate s) => MonadCrdtT s m where
  update :: Update s -> m ()


-- \s -> (updateWith u s, u, a)
newtype CrdtT s m a = CrdtT { runCrdtT :: s -> m (s, Update s, a) }

type Crdt s = CrdtT s Identity

runCrdt :: Crdt s a -> s -> (s, Update s, a)
runCrdt c = runIdentity . runCrdtT c

instance (Functor m) => Functor (CrdtT s m) where
  fmap f c = CrdtT $ \s -> fmap g (runCrdtT c s)
    where g (s, u, a) = (s, u, f a)

instance (CanUpdate s, Monad m) => Applicative (CrdtT s m) where
  pure a = CrdtT $ \s -> pure (s, mempty, a)
  cf <*> ca = CrdtT $ \s -> do
    (s1, u1, f) <- runCrdtT cf s
    (s2, u2, a) <- runCrdtT ca s1
    return (s2, u1 <> u2, f a)

instance (CanUpdate s, Monad m) => Monad (CrdtT s m) where
  return = pure
  ca >>= f = CrdtT $ \s -> do
    (s1, u1, a) <- runCrdtT ca s
    (s2, u2, b) <- runCrdtT (f a) s1
    return (s2, u1 <> u2, b)

instance (CanUpdate s) => MonadTrans (CrdtT s) where
  lift ma = CrdtT $ \s -> fmap (\a -> (s, mempty, a)) ma

instance (CanUpdate s, Monad m) => MonadReader s (CrdtT s m) where
  ask = CrdtT $ \s -> pure (s, mempty, s)
  local f c = CrdtT $ \s -> let s1 = f s
                                g (_, u, a) = (updateWith u s, u, a)
                             in fmap g $ runCrdtT c s1

instance (CanUpdate s, Monad m) => MonadCrdtT s (CrdtT s m) where
  update u = CrdtT $ \s -> pure (updateWith u s, u, ())

