{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MyMaybeT (
  MaybeT,
  runMaybeT
) where

import Control.Monad (liftM)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Trans (MonadTrans, lift)

newtype MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
}

instance (Monad m) => Monad (MaybeT m) where
  -- bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  a >>= f = MaybeT $ runMaybeT a >>= maybe (return Nothing) (runMaybeT . f)
    -- MaybeT $ do
      -- maybeA <- inner
      -- case maybeA of
      --   Just a -> runMaybeT $ f a
      --   Nothing -> return Nothing

  return = MaybeT . return . Just

  fail _ = MaybeT $ return Nothing

instance (Monad m, Functor m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  -- MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  -- m (Maybe (a -> b)) and m (Maybe a)
  (MaybeT mf) <*> (MaybeT ma) = MaybeT $ do
    maybeF <- mf
    case maybeF of
      Nothing -> return Nothing
      Just f -> do
        maybeA <- ma
        case maybeA of
          Nothing -> return Nothing
          Just a -> return . Just $ f a

instance (Functor m) => Functor (MaybeT m) where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance MonadTrans MaybeT where
  -- lift :: (MonadTrans t, Monad m) => m a -> t m a
  lift = MaybeT . liftM Just

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put = lift . put